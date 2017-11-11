module Main where

import OtherModule

-- | An example 'CBool'.
--
-- >>> toBool myCBool
-- True
myCBool :: CBool
myCBool = fromBool True

main :: IO ()
main = print (toBool myCBool)
