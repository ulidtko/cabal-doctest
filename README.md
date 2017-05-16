cabal-doctest
=============

[![Hackage](https://img.shields.io/hackage/v/cabal-doctest.svg)](https://hackage.haskell.org/package/cabal-doctest) [![Build Status](https://travis-ci.org/phadej/cabal-doctest.svg?branch=master)](https://travis-ci.org/phadej/cabal-doctest)

A `Setup.hs` helper for running `doctests`.

Example Usage
-------------

See [https://github.com/phadej/cabal-doctest/tree/master/example] for an
example package. (Note that the example requires `Cabal-1.24` or later, but
you can relax this bound safely, although running doctests won't be supported
on versions of `Cabal` older than 1.24.)

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

Additional configuration
------------------------

The `cabal-doctest` based `Setup.hs` supports few extensions fields
in `pkg.cabal` files to customise the `doctest` runner behaviour, without
customising the default `doctest.hs`.

```
test-suite doctests:
  if impl(ghc >= 8.0)
    x-doctest-options: -fdiagnostics-color=never
  x-doctest-source-dirs: test
  x-doctest-modules: Servant.Utils.LinksSpec

  ...
 ```

* `x-doctest-options` Additional arguments passed into `doctest` command.
* `x-doctest-modules` Additional modules to `doctest`. May be useful if you
  have `doctest` in test or executables (i.e not default library complonent).
* `x-doctest-src-dirs` Additional source directories to look for the modules.

Notes
-----

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

* You can use `x-doctest-options` field in `test-suite doctests` to
  pass additional flags to the `doctest`.

* For `build-type: Configure` packages, you can use
  `defaultMainAutoconfWithDoctests` function to make custom `Setup.hs` script.


Copyright
---------

Copyright 2017 Oleg Grenrus.

Available under the BSD 3-clause license.
