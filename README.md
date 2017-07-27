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

* Recent versions of `Cabal` (for instance, 2.0) can choose to build a
  package's `doctest` test suite _before_ the library. However, in order for
  `cabal-doctest` to work correctly, the library _must_ be built first, as
  `doctest` relies on the presence of generated files that are only created
  when the library is built. See
  [#19](https://github.com/phadej/cabal-doctest/issues/19).

  A hacky workaround for this problem is to depend on the library itself in a
  `doctests` test suite. See
  [the example's .cabal file](https://github.com/phadej/cabal-doctest/blob/master/example/example.cabal)
  for a demonstration. (This assumes that the test suite has the ability to
  read build artifacts from the library, a separate build component. In
  practice, this assumption holds, which is why this library works at all.)

* `custom-setup` section is supported starting from `cabal-install-1.24`.
  For older `cabal-install's` you have to install custom setup dependencies
  manually.

* `stack` respects `custom-setup` starting from version 1.3.3. Before that
  you have to use `explicit-setup-deps` setting in your `stack.yaml`.
  ([stack/GH-2094](https://github.com/commercialhaskell/stack/issues/2094))

* There is [an issue in the Cabal issue tracker](https://github.com/haskell/cabal/issues/2327)
  about adding `cabal doctest` command. After that command is implemented,
  this library will be deprecated.

* You can use `x-doctest-options` field in `test-suite doctests` to
  pass additional flags to the `doctest`.

* For `build-type: Configure` packages, you can use
  `defaultMainAutoconfWithDoctests` function to make custom `Setup.hs` script.

* If you use the default `.` in `hs-source-dirs`, then running `doctests`
  might fail with weird errors (ambigious module errors). Workaround is
  to move sources under `src/` or some non-top-level directory.

* `extensions:` field isn't supported. Upgrade your `.cabal` file to use at least
  `cabal-version: >= 1.10` and use `default-extensions` or `other-extensions`.

* If you use QuickCheck properties (`prop>`) in your doctests,
  the `test-suite doctest` should depend on `QuickCheck` and `template-haskell`.
  This is a little HACK: These dependencies aren't needed to build the
  `doctests` test-suite executable.  However, as we let `Cabal` resolve
  dependencies, we can pass the resolved (and installed!) package identifiers to
  to the `doctest` command.  This way, `QuickCheck` and `template-haskell` are
  available to `doctest`, otherwise you'll get errors like:

```
    Variable not in scope:
      mkName
        :: [Char]
           -> template-haskell-2.11.1.0:Language.Haskell.TH.Syntax.Name
```

or

```
    Variable not in scope:
      polyQuickCheck
        :: Language.Haskell.TH.Syntax.Name -> Language.Haskell.TH.Lib.ExpQ
```

Copyright
---------

Copyright 2017 Oleg Grenrus.

Available under the BSD 3-clause license.
