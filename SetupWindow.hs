module GraphicsUtils.SetupWindow where

import Control.Applicative
import Control.Monad
import Data.IORef
import System.Exit
import Graphics.UI.GLUT



setupWindow :: String -> IO ()
setupWindow name = do
  getArgsAndInitialize
  initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
  initialWindowSize $= Size 500 500
  createWindow name
  depthFunc $= Just Less
  reshapeCallback $= Just reshape


reshape :: ReshapeCallback
reshape size@(Size w h) = do
   let vp = 0.8
       aspect = fromIntegral w / fromIntegral h

   viewport $= (Position 0 0, size)
