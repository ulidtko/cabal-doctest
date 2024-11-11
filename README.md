cabal-doctest
=============

[![Hackage](https://img.shields.io/hackage/v/cabal-doctest.svg)](https://hackage.haskell.org/package/cabal-doctest) [![Haskell-CI](https://github.com/ulidtko/cabal-doctest/actions/workflows/haskell-ci.yml/badge.svg?branch=master)](https://github.com/ulidtko/cabal-doctest/actions/workflows/haskell-ci.yml)

A `Setup.hs` helper for running [doctests][doctest].

[doctest]: https://github.com/sol/doctest#readme

Why this exists
---------------

**Doctesting** is a nifty technique that stimulates 3 good things to happen:

 * library documentation gains *runnable code examples* that are also tested;
 * library test suite gains *documented usage examples* as "tests for free";
 * get both of the above for the price of one.

That's what the [doctest][] tool does — not this package! — just for clarity.
Off the shelf, `doctest` doesn't require any package management mumbo-jumbo:
you just run it on a source file with haddocks with doctests.

Issues come in when library authors and maintainers wish to integrate doctests
into CI pipelines. When doctests start to require dependencies or non-default
compiler flags: that's when it gets hairy. There, if you want `stack test` and/or
`cabal test` to run doctests too with minimal shenanigans, then read on.

Among different available approaches, this package `cabal-doctest` helps with
one, which is known as [custom setup][], `build-type: Custom` more precisely.
You should stick to the default `build-type: Simple`, unless you know what
you're doing.

In a nutshell, this custom Setup.hs shim generates a module `Build_doctests`
that allows your doctest driver `test-suite` to look like this:

```haskell
module Main where

import Build_doctests (flags, pkgs, module_sources)
import Test.Doctest (doctest)

main :: IO ()
main = doctest (flags ++ pkgs ++ module_sources)
```

More detailed examples below.

Regardless of the name, this **also works with Stack**.

For old versions of stack, cabal-install, GHC, see [caveats][#notes] below.

[custom setup]: https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html#pkg-field-build-type


Simple example
--------------

[simple example]: https://github.com/ulidtko/cabal-doctest/tree/master/simple-example
[simple-example.cabal]: https://github.com/ulidtko/cabal-doctest/tree/master/simple-example/simple-example.cabal

Follow [simple example][] for the common case of a single-library `.cabal` package with doctests.

To recap the example's code:

1. specify `build-type: Custom` in your `.cabal` file;

2. declare dependencies of `Setup.hs`:

   ```
   custom-setup
    setup-depends:
      base >= 4 && <5,
      cabal-doctest >= 1 && <1.1
   ```

   See [Notes](#notes) below for a caveat with cabal-install < 2.4.

3. Populate `Setup.hs` like so:

   ```haskell
   module Main where

   import Distribution.Extra.Doctest (defaultMainWithDoctests)

   main :: IO ()
   main = defaultMainWithDoctests "doctests"
   ```

   Assuming your test-suite is called `doctests`, this `Setup` will generate a `Build_doctests`
   module during package build. If your test-suite goes by name `foo`,
   `defaultMainWithDoctests "foo"` creates a `Build_foo` module.

4. Use the generated module in a testsuite, simply like so:

   ```haskell
   module Main where

   import Build_doctests (flags, pkgs, module_sources)
   import Data.Foldable (traverse_)
   import System.Environment (unsetEnv)
   import Test.DocTest (doctest)

   main :: IO ()
   main = do
       traverse_ putStrLn args -- optionally print arguments
       unsetEnv "GHC_ENVIRONMENT" -- see 'Notes'; you may not need this
       doctest args
     where
       args = flags ++ pkgs ++ module_sources
   ```

Ultimately, `cabal test` or `stack test` should run the doctests of your package.

Example with multiple cabal components
--------------------------------------

`cabal-doctest` also supports more exotic use cases where a `.cabal` file
contains more components with doctests than just the main library, including:

* doctests in executables,
* doctests in internal libraries (if using `Cabal-2.0` or later).

Unlike the simple example shown above, these examples involve _named_
components. You don't need to change the `Setup.hs` script to support
this use case. However, in this scenario `Build_doctests` will generate extra
copies of the `flags`, `pkgs`, and `module_sources` values for each additional
named component.

The simplest approach is to use `x-doctest-components` field in `.cabal`:
```
x-doctest-components: lib lib:internal exe:example
```

In that case, the test driver is generally:

```haskell
module Main where

import Build_doctests (Component (..), components)
import Data.Foldable (for_)
import System.Environment (unsetEnv)
import Test.DocTest (doctest)

main :: IO ()
main = for_ components $ \(Component name flags pkgs sources) -> do
    print name
    putStrLn "----------------------------------------"
    let args = flags ++ pkgs ++ sources
    for_ args putStrLn
    unsetEnv "GHC_ENVIRONMENT"
    doctest args
```

There is also a more explicit approach: if you have an executable named `foo`, then
`Build_doctest` will contain `flags_exe_foo`, `pkgs_exe_foo`, and `module_sources_exe_foo`.
If the name has hyphens in it (e.g., `my-exe`), `cabal-doctest` will convert them to
underscores (e.g., you'd get `flags_my_exe`, `pkgs_my_exe`, `module_sources_my_exe`).
Internal library `bar` values will have a `_lib_bar` suffix.

An example testsuite driver for this use case might look like this:

```haskell
module Main where

import Build_doctests
       (flags,            pkgs,            module_sources,
        flags_exe_my_exe, pkgs_exe_my_exe, module_sources_exe_my_exe)
import Data.Foldable (traverse_)
import System.Environment (unsetEnv)
import Test.DocTest

main :: IO ()
main = do
    unsetEnv "GHC_ENVRIONMENT"
    -- doctests for library
    traverse_ putStrLn libArgs
    doctest libArgs

    -- doctests for executable
    traverse_ putStrLn exeArgs
    doctest exeArgs
  where
    libArgs = flags            ++ pkgs            ++ module_sources
    exeArgs = flags_exe_my_exe ++ pkgs_exe_my_exe ++ module_sources_exe_my_exe
```

See the [multiple-components-example][].

[multiple-components-example]: https://github.com/ulidtko/cabal-doctest/tree/master/multiple-components-example


Additional configuration
------------------------

The `cabal-doctest` based `Setup.hs` supports a few extensions fields
in `pkg.cabal` files to customize the `doctest` runner behavior, without
customizing the default `doctest.hs`.

```
test-suite doctests:
  if impl(ghc >= 8.0)
    x-doctest-options: -fdiagnostics-color=never
  x-doctest-source-dirs: test
  x-doctest-modules: Servant.Utils.LinksSpec
```

* `x-doctest-options` Additional arguments passed into `doctest` command.
* `x-doctest-modules` Additional modules to `doctest`. May be useful if you
  have doctests in tests or executables (i.e not the default library component).
* `x-doctest-src-dirs` Additional source directories to look for the modules.

Notes
-----

* If support for cabal-install < 2.4 is required, you'll have to
  add `Cabal` to `setup-depends`; see issue [haskell/cabal#4288][].

* Some versions of `Cabal` (for instance, 2.0) can choose to build a
  package's `doctest` test suite _before_ the library. However, in order for
  `cabal-doctest` to work correctly, the library _must_ be built first, as
  `doctest` relies on the presence of generated files that are only created
  when the library is built. See [#19][].

  A hacky workaround for this problem is to depend on the library itself in a
  `doctests` test suite. See [simple-example.cabal][]
  for a demonstration. (This assumes that the test suite has the ability to
  read build artifacts from the library, a separate build component. In
  practice, this assumption holds, which is why this library works at all.)

* `custom-setup` section is supported starting from `cabal-install-1.24`.
  For older `cabal-install's` you have to install custom setup dependencies
  manually.

* `stack` respects `custom-setup` starting from version 1.3.3. Before that
  you have to use `explicit-setup-deps` setting in your `stack.yaml`;
  [stack#2094][].

* With base < 4.7 (GHC < 7.8, pre-2014), `System.Environment.unsetEnv` function
  will need to be imported from `base-compat` library. It is already in transitive
  dependencies of `doctest`. Simply declare the dependency upon `base-compat`, and
  then `import System.Environment.Compat (unsetEnv)` if you need that old GHC.

* You can use `x-doctest-options` field in `test-suite doctests` to
  pass additional flags to the `doctest`.

* For `build-type: Configure` packages, you can use
  `defaultMainAutoconfWithDoctests` function to make custom `Setup.hs` script.

* If you use the default `.` in `hs-source-dirs`, then running `doctests`
  might fail with weird errors (ambiguous module errors). Workaround is
  to move sources under `src/` or some non-top-level directory.

* The `extensions:` field isn't supported. Upgrade your `.cabal` file to use at least
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

* From version 2, Stack sets the `GHC_ENVIRONMENT` variable, and GHC
  (as invoked by `doctest`) will pick that up. This is undesirable:
  `cabal-doctest` passes all the necessary information on the command
  line already, and can lead to ambiguous module errors as GHC will
  load the environment in addition to what `cabal-doctest` instructs
  it to.

  Hence, `cabal-doctest` tells GHC to ignore package environments
  altogether on the command line. However, this is only possible since
  GHC 8.2. If you are using `cabal-doctest` with Stack 2 and GHC 8.0
  or earlier and seeing ambiguous module errors or other mysterious
  failures, try manually unsetting `GHC_ENVIRONMENT` before invoking
  `doctest`.

 * If you are on Nix. `doctest` will not pick up your version of GHC if you
   don't point it towards it, and therefore will result in "cannot satisfy -package-id" errors.
   You will need to set `NIX_GHC` and `NIX_GHC_LIBDIR` within your environment in order
   for doctest to pick up your GHC. Put the following in `shell.nix` and run `nix-shell`.

   ```nix
   # shell.nix
   { pkgs ? import <nixpkgs> {} }:
   let
     myHaskell = (pkgs.haskellPackages.ghcWithHoogle (p: with p; [
       # Put your dependencies here
       containers
       hslogger
     ]));
   in
   pkgs.mkShell {
     name = "myPackage";

     # These environment variables are important. Without these,
     # doctest doesn't pick up nix's version of ghc, and will fail
     # claiming it can't find your dependencies
     shellHook = ''
       export NIX_GHC=${myHaskell}/bin/ghc
       export NIX_GHC_LIBDIR=${myHaskell}/lib/ghc-8.10.7
     '';
     buildInputs = with pkgs; [
       myHaskell
     ];
   }
   ```

[#19]: https://github.com/ulidtko/cabal-doctest/issues/19
[haskell/cabal#4288]: https://github.com/haskell/cabal/issues/4288
[stack#2094]: https://github.com/commercialhaskell/stack/issues/2094

Copyright
---------

Copyright 2017 Oleg Grenrus.

With contributions from:
 * Ryan Scott
 * Andreas Abel
 * Max Ulidtko

Available under the BSD 3-clause license.
