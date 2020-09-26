# kawakudari-pfh

This project implements part of the [std15.h](https://github.com/IchigoJam/c4ij/blob/master/src/std15.h) API (from [c4ij](https://github.com/IchigoJam/c4ij)) with [processing-for-haskell](https://hackage.haskell.org/package/processing-for-haskell), and [Kawakudari Game](https://ichigojam.github.io/print/en/KAWAKUDARI.html) on top of it.

It will allow programming for [IchigoJam](https://ichigojam.net/index-en.html)-like targets that display [IchigoJam FONT](https://mitsuji.github.io/ichigojam-font.json/) on screen using a Haskell programming language.
```
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
      IJ.scroll std15 DirUp
      sc <- IJ.scr std15 x 5
      when (sc /= '\0') $ do
        IJ.locate std15 0 23
        IJ.putstr std15 "Game Over..."
        IJ.putnum std15 fc
        liftIO $ writeIORef running False
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

## License
[![Creative Commons License](https://i.creativecommons.org/l/by/4.0/88x31.png)](http://creativecommons.org/licenses/by/4.0/)
[CC BY](https://creativecommons.org/licenses/by/4.0/) [mitsuji.org](https://mitsuji.org)

This work is licensed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/).
