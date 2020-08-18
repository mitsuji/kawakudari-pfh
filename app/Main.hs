module Main where

import Graphics.Proc
import IchigoJam
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

main :: IO ()
main = runProc $ def { procSetup      = setup
                     , procUpdate     = update
                     , procDraw       = draw
                     , procKeyPressed = keyPressed
                     }

type State = (Std15, Int, IORef Bool)

setup :: Pio State
setup = do
  size (512, 384)
  std15 <- newStd15 512 384 32 24
  refRunning <- liftIO $ newIORef True
  return (std15,15,refRunning)

update :: Update State
update state@(std15,x,refRunning) = do
  running <- liftIO $ readIORef refRunning
  when running $ do
    fc <- frameCount
    when (fc `mod` 5 == 0) $ do
      locate std15 x 5
      putc std15 '0'
      floor <$> random 32 >>= \rnd -> locate std15 rnd 23
      putc std15 '*'
      scroll std15
      cc <- scr std15 x 5
      when (cc /= '\0') $ liftIO $ writeIORef refRunning False
  return state

draw :: State -> Draw
draw (std15,_,_) = pAppletDraw std15

keyPressed :: Update State
keyPressed (std15,x,refRunning) = do
  k <- key
  let x' = case k of
        SpecialKey KeyLeft  -> x-1
        SpecialKey KeyRight -> x+1
        _ -> x
  return (std15,x',refRunning)
  
