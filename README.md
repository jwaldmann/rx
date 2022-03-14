rx: Computations with regular tree expressions.
==============================================

build

```
cabal install -w/opt/ghc/ghc-7.0.4/bin/ghc 
```

run
```
rx examples/basic.lit examples/check.lit
```

build documentation
```
cd doc ; make
```

## History

* 1997: I wrote this for my [PhD thesis](https://www.imn.htwk-leipzig.de/~waldmann/pub/#diss). Way back!
  Comments in the source indicate that I compiled with both `ghc` and `hbc`.
* 2014: cabal-ized version. Using package `haskell98` to get flat-namespace libraries.
* 2022: re-cabalized.  It can be built with (recent cabal-install and) GHC 7.0.4, and 7.4.2.
