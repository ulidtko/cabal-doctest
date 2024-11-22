module Main where

import Data.Version (showVersion)

import OtherModule
import qualified Paths_multiple_components_example as Paths

-- $setup
--
-- This is a special block to run before each doctest (group).
--
-- >>> import Data.Version.Compat (makeVersion)
-- >>> let setupDummy = [1, 2, 3]

-- | An example 'CBool'.
--
-- >>> toBool myCBool
-- True
myCBool :: CBool
myCBool = fromBool True

-- | Doctest examples.
--
-- We can assert a thing about our package version, obtainable from the special
-- cabal-generated Paths module:
--
-- >>> showVersion Paths.version
-- "1"
--
-- We can assert a thing using names from the $setup block:
--
-- >>> showVersion (makeVersion setupDummy)
-- "1.2.3"
--
main :: IO ()
main = do
    print (showVersion Paths.version)
    print (toBool myCBool)
