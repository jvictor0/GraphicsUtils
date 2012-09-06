module Shader where

import Control.Applicative
import Control.Monad
import Data.IORef
import System.Exit
import Graphics.UI.GLUT


installShader :: FilePath -> IO ()
installShader shadr = do 
  checkGLSLSupport
  vs <- readAndCompileShader $ shadr ++ ".vert"
  fs <- readAndCompileShader $ shadr ++ ".frag"
  installShaders [vs] [fs]

readAndCompileShader :: Shader s => FilePath -> IO s
readAndCompileShader filePath = do
   src <- readFile filePath
   [shader] <- genObjectNames 1
   shaderSource shader $= [src]
   compileShader shader
   reportErrors
   ok <- get (compileStatus shader)
   infoLog <- get (shaderInfoLog shader)
   mapM_ putStrLn ["Shader info log for '" ++ filePath ++ "':", infoLog, ""]
   unless ok $ do
      deleteObjectNames [shader]
      ioError (userError "shader compilation failed")
   return shader

installShaders :: [VertexShader] -> [FragmentShader] -> IO ()
installShaders vs fs = do
   [shadeProg] <- genObjectNames 1
   attachedShaders shadeProg $= (vs, fs)
   linkProgram shadeProg
   reportErrors
   ok <- get (linkStatus shadeProg)
   infoLog <- get (programInfoLog shadeProg)
   mapM_ putStrLn ["Program info log:", infoLog, ""]
   unless ok $ do
      deleteObjectNames [shadeProg]
      ioError (userError "linking failed")
   currentProgram $= Just shadeProg
   let setUniform var val = do
       location <- get (uniformLocation shadeProg var)
       reportErrors
       uniform location $= val
   return ()

checkGLSLSupport :: IO ()
checkGLSLSupport = do
   version <- get (majorMinor glVersion)
   unless (version >= (2,0)) $ do
      extensions <- get glExtensions
      unless ("GL_ARB_shading_language_100" `elem` extensions) $
         ioError (userError "No GLSL support found.")

