module Main where

import Build_doctests
       (flags,        pkgs,        module_sources,
        flags_my_exe, pkgs_my_exe, module_sources_my_exe)
import Data.Foldable (traverse_)
import Test.DocTest

main :: IO ()
main = do
    -- doctests for library
    traverse_ putStrLn libArgs
    doctest libArgs

    -- doctests for executable
    traverse_ putStrLn exeArgs
    doctest exeArgs
  where
    libArgs = flags        ++ pkgs        ++ module_sources
    exeArgs = flags_my_exe ++ pkgs_my_exe ++ module_sources_my_exe
