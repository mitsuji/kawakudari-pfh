module Main where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Graphics.Proc
import qualified IchigoJam as IJ

main :: IO ()
main = runProc $ def { procSetup      = setup
                     , procUpdate     = update
                     , procDraw       = draw
                     , procKeyPressed = keyPressed
                     }

type State = (IJ.Std15, IORef Bool, Int)

setup :: Pio State
setup = do
  size (512, 384)
  std15 <- IJ.newStd15 512 384 32 24
  running <- liftIO $ newIORef True
  return (std15,running,15)

update :: Update State
update state@(std15,running,x) = do
  isRunning <- liftIO $ readIORef running
  when isRunning $ do
    fc <- frameCount
    when (fc `mod` 5 == 0) $ do
      IJ.locate std15 x 5
      IJ.putc std15 '0'
      random 32 >>= \rnd -> IJ.locate std15 (floor rnd) 23
      IJ.putc std15 '*'
      IJ.scroll std15
      sc <- IJ.scr std15 x 5
      when (sc /= '\0') $ liftIO $ writeIORef running False
  return state

draw :: State -> Draw
draw (std15,_,_) = IJ.drawScreen std15

keyPressed :: Update State
keyPressed (std15,running,x) = do
  k <- key
  let x' = case k of
        SpecialKey KeyLeft  -> x-1
        SpecialKey KeyRight -> x+1
        _ -> x
  return (std15,running,x')
  
