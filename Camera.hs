module GraphicsUtils.Camera where

import Control.Applicative
import Control.Monad
import Data.IORef
import System.Exit
import Graphics.UI.GLUT


data Camera = Camera GLdouble GLdouble GLdouble deriving (Eq,Show)
type CamState = IORef Camera

initialCam = Camera 4 0 0

dr = 0.1 :: GLdouble
dtheta = 0.1 :: GLdouble
dphi = 0.1 :: GLdouble

setupCamera state = do
  (Camera r theta phi) <- readIORef state
  matrixMode $= Projection
  loadIdentity
  perspective 45 1 0.5 100

  matrixMode $= Modelview 0
  loadIdentity
  translate (Vector3 0 0 (-r))
  rotate theta (Vector3 0 1 0)
  let thetarad = theta * pi / 180
  rotate phi (Vector3 (cos thetarad) 0 (- sin thetarad))
  
zoomIn (Camera r theta phi) = (Camera (r+dr) theta phi)
zoomOut (Camera r theta phi) = (Camera (r-dr) theta phi)
rotateLeft (Camera r theta phi) = (Camera r (theta+dtheta) phi)
rotateRight (Camera r theta phi) = (Camera r (theta-dtheta) phi)
rotateUp (Camera r theta phi) = (Camera r theta (phi+dphi))
rotateDown (Camera r theta phi) = (Camera r theta (phi-dphi))

camKeyboard :: CamState -> KeyboardMouseCallback
camKeyboard state key keyState mods _ = do
   postRedisplay Nothing
   case (key, keyState) of
      (Char 'w', Down) -> modifyIORef state rotateUp
      (Char 's', Down) -> modifyIORef state rotateDown
      (SpecialKey KeyLeft, Down) -> modifyIORef state rotateLeft
      (SpecialKey KeyRight, Down) -> modifyIORef state rotateRight
      (SpecialKey KeyUp, Down) -> modifyIORef state zoomIn
      (SpecialKey KeyDown, Down) -> modifyIORef state zoomOut
      (_, _) -> return ()


withCamera displayFun keyboardFun = do
  state <- newIORef initialCam
  displayCallback $= (setupCamera state >> displayFun)
  keyboardMouseCallback $= Just (camKeyboard state >> keyboardFun)
