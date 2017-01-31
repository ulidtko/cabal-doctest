cabal-doctest
-------------

[![Build Status](https://travis-ci.org/phadej/cabal-doctest.svg?branch=master)](https://travis-ci.org/phadej/cabal-doctest)

A `Setup.hs` helper for running `doctests`.

Example Usage
=============

To use this library in your `Setup.hs`, you should specify a `custom-setup`
section in your `.cabal` file. For example:

```
custom-setup
 setup-depends:
   base >= 4 && <5,
   Cabal >= 1.10 && <2.1,
   cabal-doctest >= 1 && <1.1
```

You'll also need to specify `build-type: Custom` at the top of the `.cabal`
file. Now put this into your `Setup.hs` file:

```haskell
module Main where

import Distribution.Extra.Doctest (generateBuildModule)
import Distribution.Simple (defaultMainWithHooks, UserHooks(..), simpleUserHooks)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule "doctests" flags pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  }
```

When you build your project, this `Setup` will generate a `Build_doctests`
module. To use it in a testsuite, simply do this:

```haskell
module Main where

import Build_doctests (flags, pkgs, module_sources)
import Data.Foldable (traverse_)
import Test.Doctest (doctest)

main :: IO ()
main = do
    traverse_ putStrLn args -- optionally print arguments
    doctest args
  where
    args = flags ++ pkgs ++ module_sources
```

Copyright
=========

Copyright 2017 Oleg Grenrus.

Available under the BSD 3-clause license.
