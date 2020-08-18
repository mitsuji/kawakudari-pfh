# kawakudari-pfh

This project implements part of the [std15.h](https://github.com/IchigoJam/c4ij/blob/master/src/std15.h) API (from [c4ij](https://github.com/IchigoJam/c4ij)) with [processing-for-haskell](https://hackage.haskell.org/package/processing-for-haskell), and [Kawakudari Game](https://ichigojam.github.io/print/en/KAWAKUDARI.html) on top of it.

It will allow programming for [IchigoJam](https://ichigojam.net/index-en.html)-like targets using a Haskell programming language.
```
setup = do
  size (512, 384)
  std15 <- newStd15 512 384 32 24
  refRunning <- liftIO $ newIORef True
  return (std15,15,refRunning)

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

draw (std15,_,_) = pAppletDraw std15

keyPressed (std15,x,refRunning) = do
  k <- key
  let x' = case k of
        SpecialKey KeyLeft  -> x-1
        SpecialKey KeyRight -> x+1
        _ -> x
  return (std15,x',refRunning)
```

## Prerequisite

* This project using programming language Haskell, so you need [Stack](https://docs.haskellstack.org/en/stable/README/) build tool properly installd to run example code.


## How to use

To just run example
```
$ stack run
```

To build executeble
```
$ stack build
```

To install and run example
```
$ stack install
$ ~/.local/bin/kawakudari-pfh-exe
```
