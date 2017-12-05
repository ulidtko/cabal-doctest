module Main where

import Build_doctests (Component (..), components)
import Data.Foldable (for_)
import Test.DocTest (doctest)

main :: IO ()
main = for_ components $ \(Component name flags pkgs sources) -> do
    print name
    putStrLn "----------------------------------------"
    let args = flags ++ pkgs ++ sources
    for_ args putStrLn
    doctest args
