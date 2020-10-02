module Main where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Graphics.Proc
import IchigoJam (Std15,Direction(..))
import qualified IchigoJam as IJ

main :: IO ()
main = runProc $ def { procSetup      = setup
                     , procUpdate     = update
                     , procDraw       = draw
                     , procKeyPressed = keyPressed
                     }

type State = (Std15, Word, IORef Bool, Int)

setup :: Pio State
setup = do
  size (512, 384)
  std15 <- IJ.newStd15 512 384 32 24
  running <- liftIO $ newIORef True
  return (std15,0,running,15)

update :: Update State
update (std15,frame,running,x) = do
  isRunning <- liftIO $ readIORef running
  when isRunning $ do
    when (frame `mod` 2 == 0) $ do
      IJ.locate std15 x 5
      IJ.putc std15 '0'
      random 32 >>= \rnd -> IJ.locate std15 (floor rnd) 23
      IJ.putc std15 '*'
      IJ.scroll std15 DirUp
      sc <- IJ.scr std15 x 5
      when (sc /= '\0') $ do
        IJ.locate std15 0 23
        IJ.putstr std15 "Game Over..."
        IJ.putnum std15 $ fromIntegral frame
        liftIO $ writeIORef running False
  return (std15,frame+1,running,x)

draw :: State -> Draw
draw (std15,_,_,_) = IJ.drawScreen std15

keyPressed :: Update State
keyPressed (std15,frame,running,x) = do
  k <- key
  let x' = case k of
        SpecialKey KeyLeft  -> x-1
        SpecialKey KeyRight -> x+1
        _ -> x
  return (std15,frame,running,x')
  
