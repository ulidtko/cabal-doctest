# 1.0.11 -- unreleased

* Support Cabal 3.14.0.0. [cabal-doctest#85][].

[cabal-doctest#85]: https://github.com/ulidtko/cabal-doctest/issues/85

# 1.0.10 -- 2024-06-26

* Maintainership hand-over. See [cabal-doctest#79][].
* Support GHC 9.4, 9.6, 9.8, 9.10.
* Drop support & CI for GHC < 8.0.

[cabal-doctest#79]: https://github.com/ulidtko/cabal-doctest/issues/79

# 1.0.9 -- 2021-11-07

* Support `GHC-9.2`, `base-4.16`, and `Cabal-3.6` (thanks Alistair Burrowes).

# 1.0.8 -- 2019-10-02

* Pass `-package-env=-` when compiler supports it.
* Amend examples to `unsetEnv "GHC_ENVIRONMENT"`.

# 1.0.7 -- 2019-08-26

* Make `Distribution.Extra.Doctest` `-Wall`-clean.
* Support `GHC-8.8`, `base-4.13`, and `Cabal-3.0`.

# 1.0.6 -- 2018-01-28

* Hook `haddock` build too. Fixes issue when `haddock` fails, as
  `Build_doctests` isn't generated.

# 1.0.5 -- 2018-01-26

* Add a hack so `Build_doctests` module is automatically added to
  to `other-modules` and `autogen-modules` when compiled with Cabal-2.0.
  Thanks to that, we don't get warnings because of `-Wmissing-home-modules`.

# 1.0.4 -- 2017-12-05

* Add support for doctests in executables and (with `Cabal-2.0` or later)
  internal libraries. Refer to the `README` for more details.

# 1.0.3 -- 2017-11-02

* Add an explicit `Prelude` import to `Build_doctests`.

# 1.0.2 -- 2017-05-16

* Add `defaultMainAutoconfWithDoctests` and `addDoctestsUserHook`.
* Add support for `.hsc` and other preprocessed files
  ([#8](https://github.com/phadej/cabal-doctest/issues/8)).
* Add support for `x-doctest-source-dirs` and `x-doctest-modules`.

# 1.0.1 -- 2017-05-05

* Add support for `x-doctest-options` cabal-file field.
* Proper support for `GHC-8.2.1` and `Cabal-2.0.0.0`.
* Add support to `default-extensions` in library.

# 1  -- 2017-01-31

* First version. Released on an unsuspecting world.
