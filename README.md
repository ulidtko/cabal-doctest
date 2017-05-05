cabal-doctest
-------------

[![Hackage](https://img.shields.io/hackage/v/cabal-doctest.svg)](https://hackage.haskell.org/package/cabal-doctest) [![Build Status](https://travis-ci.org/phadej/cabal-doctest.svg?branch=master)](https://travis-ci.org/phadej/cabal-doctest)

A `Setup.hs` helper for running `doctests`.

Example Usage
=============

See [https://github.com/phadej/cabal-doctest/tree/master/example] for an example package.

To use this library in your `Setup.hs`, you should specify a `custom-setup`
section in your `.cabal` file. For example:

```
custom-setup
 setup-depends:
   base >= 4 && <5,
   Cabal,
   cabal-doctest >= 1 && <1.1
```

/Note:/ `Cabal` dependency is needed because of
[Cabal/GH-4288](https://github.com/haskell/cabal/issues/4288) bug.

You'll also need to specify `build-type: Custom` at the top of the `.cabal`
file. Now put this into your `Setup.hs` file:

```haskell
module Main where

import Distribution.Extra.Doctest (defaultMainWithDoctests)

main :: IO ()
main = defaultMainWithDoctests "doctests"
```

When you build your project, this `Setup` will generate a `Build_doctests`
module. To use it in a testsuite, simply do this:

```haskell
module Main where

import Build_doctests (flags, pkgs, module_sources)
import Data.Foldable (traverse_)
import Test.DocTest (doctest)

main :: IO ()
main = do
    traverse_ putStrLn args -- optionally print arguments
    doctest args
  where
    args = flags ++ pkgs ++ module_sources
```

Notes
=====

* `custom-setup` section is supported starting from `cabal-install-1.24`.
  For older `cabal-install's` you have to install custom setup dependencies
  manually.

* `stack` respects `custom-setup` starting from version 1.3.3. Before that
  you have to use `explicit-setup-deps` setting in your `stack.yaml`.
  ([stack/GH-2094](https://github.com/commercialhaskell/stack/issues/2094))

* There is [an issue in the Cabal issue tracker](https://github.com/haskell/cabal/issues/2327)
  about adding `cabal doctest` command. After that command is implemented,
  this library will be deprecated.

* If your library contains `cbits`, you might need to depend on the library
  itself in `doctests` test-suite. We aren't sure whether this a bug or not.
  See [#5 issue](https://github.com/phadej/cabal-doctest/issues/5) for longer
  explanation.

Copyright
=========

Copyright 2017 Oleg Grenrus.

Available under the BSD 3-clause license.
