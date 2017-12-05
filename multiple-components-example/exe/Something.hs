module Main where

import OtherModule
import Data.Version (showVersion)
import qualified Paths_multiple_components_example as Paths

-- | An example 'CBool'.
--
-- >>> toBool myCBool
-- True
myCBool :: CBool
myCBool = fromBool True

-- | Example of paths
--
-- >>> showVersion Paths.version
-- "1"
main :: IO ()
main = do
    print (showVersion Paths.version)
    print (toBool myCBool)
