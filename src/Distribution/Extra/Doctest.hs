{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The provided 'generateBuildModule' generates 'Build_doctests' module.
-- That module exports enough configuration, so your doctests could be simply
--
-- @
-- module Main where
--
-- import Build_doctests (flags, pkgs, module_sources)
-- import Data.Foldable (traverse_)
-- import Test.Doctest (doctest)
--
-- main :: IO ()
-- main = do
--     traverse_ putStrLn args -- optionally print arguments
--     doctest args
--   where
--     args = flags ++ pkgs ++ module_sources
-- @
--
-- To use this library in the @Setup.hs@, you should specify a @custom-setup@
-- section in the cabal file, for example:
--
-- @
-- custom-setup
--  setup-depends:
--    base >= 4 && <5,
--    cabal-doctest >= 1 && <1.1
-- @
--
-- /Note:/ you don't need to depend on @Cabal@  if you use only
-- 'defaultMainWithDoctests' in the @Setup.hs@.
--
module Distribution.Extra.Doctest (
    defaultMainWithDoctests,
    defaultMainAutoconfWithDoctests,
    addDoctestsUserHook,
    doctestsUserHooks,
    generateBuildModule,
    amendBuildModule,
    ) where

-- Hacky way to suppress few deprecation warnings.
#if MIN_VERSION_Cabal(1,24,0)
#define InstalledPackageId UnitId
#endif

import Control.Monad
       (when)
import Data.List
       (nub)
import Data.String
       (fromString)
import Distribution.Package
       (InstalledPackageId, Package (..), PackageId, packageVersion)
import Distribution.PackageDescription
       (BuildInfo (..), GenericPackageDescription (..), Library (..),
       PackageDescription, TestSuite (..))
import Distribution.Simple
       (UserHooks (..), autoconfUserHooks, defaultMainWithHooks,
       simpleUserHooks)
import Distribution.Simple.Compiler
       (PackageDB (..), showCompilerId)
import Distribution.Simple.LocalBuildInfo
       (ComponentLocalBuildInfo (componentPackageDeps), LocalBuildInfo,
       compiler, withLibLBI, withPackageDB, withTestLBI)
import Distribution.Simple.Setup
       (BuildFlags (buildDistPref, buildVerbosity), fromFlag)
import Distribution.Simple.Utils
       (createDirectoryIfMissingVerbose, rewriteFile)
import Distribution.Text
       (display, simpleParse)
import System.FilePath
       ((</>))

#if MIN_VERSION_Cabal(1,25,0)
import Distribution.Simple.BuildPaths
       (autogenComponentModulesDir)
#else
import Distribution.Simple.BuildPaths
       (autogenModulesDir)
#endif
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.MungedPackageId
       (MungedPackageId)
#endif

#if MIN_VERSION_Cabal(2,0,0)
import Data.List
       (find, isSuffixOf)
import Distribution.PackageDescription
       (CondTree (..))
import Distribution.PackageDescription.Parse
       (readGenericPackageDescription)
import Distribution.Verbosity
       (silent)
import System.Directory
       (listDirectory)
#endif

#if MIN_VERSION_directory(1,2,2)
import System.Directory
       (makeAbsolute)
#else
import System.Directory
       (getCurrentDirectory)
import System.FilePath
       (isAbsolute)

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute p | isAbsolute p = return p
               | otherwise    = do
    cwd <- getCurrentDirectory
    return $ cwd </> p
#endif

-- | A default main with doctests:
--
-- @
-- import Distribution.Extra.Doctest
--        (defaultMainWithDoctests)
--
-- main :: IO ()
-- main = defaultMainWithDoctests "doctests"
-- @
defaultMainWithDoctests
    :: String  -- ^ doctests test-suite name
    -> IO ()
defaultMainWithDoctests = defaultMainWithHooks . doctestsUserHooks

-- | Like 'defaultMainWithDoctests', for 'build-type: Configure' packages.
--
-- @since 1.0.2
defaultMainAutoconfWithDoctests
    :: String  -- ^ doctests test-suite name
    -> IO ()
defaultMainAutoconfWithDoctests n =
    defaultMainWithHooks (addDoctestsUserHook n autoconfUserHooks)

-- | 'simpleUserHooks' with 'generateBuildModule' prepended to the 'buildHook'.
doctestsUserHooks
    :: String  -- ^ doctests test-suite name
    -> UserHooks
doctestsUserHooks testsuiteName =
    addDoctestsUserHook testsuiteName simpleUserHooks

-- |
--
-- @since 1.0.2
addDoctestsUserHook :: String -> UserHooks -> UserHooks
addDoctestsUserHook testsuiteName uh = uh
    { buildHook = \pkg lbi hooks flags -> do
       generateBuildModule testsuiteName flags pkg lbi
       buildHook uh pkg lbi hooks flags
#if MIN_VERSION_Cabal(2,0,0)
    , readDesc = do
        paths <- listDirectory "."
        case find (isSuffixOf ".cabal") paths of
            Nothing -> return Nothing
            Just fp -> do
                gpd <- readGenericPackageDescription silent fp
                let gpd' = amendBuildModule testsuiteName gpd
                return (Just gpd')
#endif
    }

-- | Add @Build_doctests@ module to other-modules in the doctest test-suite.
--
-- TBD
--
-- @since 1.0.3
amendBuildModule
    :: String -- ^ doctests test-suite name
    -> GenericPackageDescription
    -> GenericPackageDescription
#if MIN_VERSION_Cabal(2,0,0)
amendBuildModule testSuiteName gpd = gpd
    { condTestSuites = map f (condTestSuites gpd)
    }
  where
    f (name, condTree)
        | name == fromString testSuiteName = (name, condTree')
        | otherwise                        = (name, condTree)
      where
        -- I miss 'lens'
        testSuite = condTreeData condTree
        bi = testBuildInfo testSuite
        om = otherModules bi
        am = autogenModules bi

        -- append
        om' = fromString "Build_doctests" : om
        am' = fromString "Build_doctests" : am

        bi' = bi { otherModules = om', autogenModules = am' }
        testSuite' = testSuite { testBuildInfo = bi' }
        condTree' = condTree { condTreeData = testSuite' }
#else
amendBuildModule _ gpd = gpd
#endif

-- | Generate a build module for the test suite.
--
-- @
-- import Distribution.Simple
--        (defaultMainWithHooks, UserHooks(..), simpleUserHooks)
-- import Distribution.Extra.Doctest
--        (generateBuildModule)
--
-- main :: IO ()
-- main = defaultMainWithHooks simpleUserHooks
--     { buildHook = \pkg lbi hooks flags -> do
--         generateBuildModule "doctests" flags pkg lbi
--         buildHook simpleUserHooks pkg lbi hooks flags
--     }
-- @
generateBuildModule
    :: String -- ^ doctests test-suite name
    -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule testSuiteName flags pkg lbi = do
  let verbosity = fromFlag (buildVerbosity flags)
  let distPref = fromFlag (buildDistPref flags)

  -- Package DBs
  let dbStack = withPackageDB lbi ++ [ SpecificPackageDB $ distPref </> "package.conf.inplace" ]
  let dbFlags = "-hide-all-packages" : packageDbArgs dbStack

  withLibLBI pkg lbi $ \lib libcfg -> do
    let libBI = libBuildInfo lib

    -- modules
    let modules = exposedModules lib ++ otherModules libBI
    -- it seems that doctest is happy to take in module names, not actual files!
    let module_sources = modules

    -- We need the directory with library's cabal_macros.h!
#if MIN_VERSION_Cabal(1,25,0)
    let libAutogenDir = autogenComponentModulesDir lbi libcfg
#else
    let libAutogenDir = autogenModulesDir lbi
#endif

    -- Lib sources and includes
    iArgs' <- mapM (fmap ("-i"++) . makeAbsolute)
        $ libAutogenDir            -- autogenerated files
        : (distPref ++ "/build")   -- preprocessed files (.hsc -> .hs); "build" is hardcoded in Cabal.
        : hsSourceDirs libBI
    includeArgs <- mapM (fmap ("-I"++) . makeAbsolute) $ includeDirs libBI
    -- We clear all includes, so the CWD isn't used.
    let iArgs = "-i" : iArgs'

    -- default-extensions
    let extensionArgs = map (("-X"++) . display) $ defaultExtensions libBI

    -- CPP includes, i.e. include cabal_macros.h
    let cppFlags = map ("-optP"++) $
            [ "-include", libAutogenDir ++ "/cabal_macros.h" ]
            ++ cppOptions libBI

    withTestLBI pkg lbi $ \suite suitecfg -> when (testName suite == fromString testSuiteName) $ do
      let testBI = testBuildInfo suite

      -- TODO: `words` is not proper parser (no support for quotes)
      let additionalFlags = maybe [] words
            $ lookup "x-doctest-options"
            $ customFieldsBI testBI

      let additionalModules = maybe [] words
            $ lookup "x-doctest-modules"
            $ customFieldsBI testBI

      let additionalDirs' = maybe [] words
            $ lookup "x-doctest-source-dirs"
            $ customFieldsBI testBI
      additionalDirs <- mapM (fmap ("-i" ++) . makeAbsolute) additionalDirs'


      -- get and create autogen dir
#if MIN_VERSION_Cabal(1,25,0)
      let testAutogenDir = autogenComponentModulesDir lbi suitecfg
#else
      let testAutogenDir = autogenModulesDir lbi
#endif
      createDirectoryIfMissingVerbose verbosity True testAutogenDir

      -- write autogen'd file
      rewriteFile (testAutogenDir </> "Build_doctests.hs") $ unlines
        [ "module Build_doctests where"
        , ""
        -- -package-id etc. flags
        , "pkgs :: [String]"
        , "pkgs = " ++ (show $ formatDeps $ testDeps libcfg suitecfg)
        , ""
        , "flags :: [String]"
        , "flags = " ++ show (concat
          [ iArgs
          , additionalDirs
          , includeArgs
          , dbFlags
          , cppFlags
          , extensionArgs
          , additionalFlags
          ])
        , ""
        , "module_sources :: [String]"
        , "module_sources = " ++ show (map display module_sources ++ additionalModules)
        ]
  where
    -- we do this check in Setup, as then doctests don't need to depend on Cabal
    isOldCompiler = maybe False id $ do
      a <- simpleParse $ showCompilerId $ compiler lbi
      b <- simpleParse "7.5"
      return $ packageVersion (a :: PackageId) < b

    formatDeps = map formatOne
    formatOne (installedPkgId, pkgId)
      -- The problem is how different cabal executables handle package databases
      -- when doctests depend on the library
      --
      -- If the pkgId is current package, we don't output the full package-id
      -- but only the name
      --
      -- Because of MungedPackageId we compare display version of identifiers
      -- not the identifiers themfselves.
      | display (packageId pkg) == display pkgId = "-package=" ++ display pkgId
      | otherwise              = "-package-id=" ++ display installedPkgId

    -- From Distribution.Simple.Program.GHC
    packageDbArgs :: [PackageDB] -> [String]
    packageDbArgs | isOldCompiler = packageDbArgsConf
                  | otherwise     = packageDbArgsDb

    -- GHC <7.6 uses '-package-conf' instead of '-package-db'.
    packageDbArgsConf :: [PackageDB] -> [String]
    packageDbArgsConf dbstack = case dbstack of
      (GlobalPackageDB:UserPackageDB:dbs) -> concatMap specific dbs
      (GlobalPackageDB:dbs)               -> ("-no-user-package-conf")
                                           : concatMap specific dbs
      _ -> ierror
      where
        specific (SpecificPackageDB db) = [ "-package-conf=" ++ db ]
        specific _                      = ierror
        ierror = error $ "internal error: unexpected package db stack: "
                      ++ show dbstack

    -- GHC >= 7.6 uses the '-package-db' flag. See
    -- https://ghc.haskell.org/trac/ghc/ticket/5977.
    packageDbArgsDb :: [PackageDB] -> [String]
    -- special cases to make arguments prettier in common scenarios
    packageDbArgsDb dbstack = case dbstack of
      (GlobalPackageDB:UserPackageDB:dbs)
        | all isSpecific dbs              -> concatMap single dbs
      (GlobalPackageDB:dbs)
        | all isSpecific dbs              -> "-no-user-package-db"
                                           : concatMap single dbs
      dbs                                 -> "-clear-package-db"
                                           : concatMap single dbs
     where
       single (SpecificPackageDB db) = [ "-package-db=" ++ db ]
       single GlobalPackageDB        = [ "-global-package-db" ]
       single UserPackageDB          = [ "-user-package-db" ]
       isSpecific (SpecificPackageDB _) = True
       isSpecific _                     = False

-- | In compat settings it's better to omit the type-signature
testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo
#if MIN_VERSION_Cabal(2,0,0)
         -> [(InstalledPackageId, MungedPackageId)]
#else
         -> [(InstalledPackageId, PackageId)]
#endif
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys
