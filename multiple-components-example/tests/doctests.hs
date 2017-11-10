module Main where

import Build_doctests
       (flags,     pkgs,     module_sources,
        flags_exe, pkgs_exe, module_sources_exe)
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
    libArgs = flags     ++ pkgs     ++ module_sources
    exeArgs = flags_exe ++ pkgs_exe ++ module_sources_exe
