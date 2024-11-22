#!/usr/bin/env cabal
{- cabal:
build-depends: base, aeson, yaml, http-client, http-client-tls, http-types
-}
{-# LANGUAGE GHC2021, OverloadedStrings #-}

{- | Fetch Stackage LTS -> GHC version map, print as YAML. -}
module Main where

import Control.Monad ((<=<), unless, forM, forM_)
import Data.Bifunctor (first)
import Data.Function (on)
import Data.Functor ((<&>))
import System.Exit (die)

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (status200)

newtype LtsEntries = LtsEntries [(String, String)] -- {"lts-20": "lts-20.26"}
  deriving Show
instance FromJSON LtsEntries where
  parseJSON = withObject "LtsEntries" $
    pure . LtsEntries . map (first Key.toString) . KM.toList <=< mapM parseJSON

ghcVersionEndpoint :: String -> Request
ghcVersionEndpoint tag = parseRequest_ $
  "https://www.stackage.org/" <> tag <> "/ghc-major-version"

main :: IO ()
main = do
  gConnPool <- newTlsManager -- reuses one connection for all requests
  -- fetch list of LTS snapshots
  req <- parseUrlThrow "https://www.stackage.org/download/lts-snapshots.json"
  resp <- httpLbs req gConnPool
  unless (responseStatus resp == status200) . die $
    "Fetch of Stackage lts-snapshots.json gave " ++ show resp
  LtsEntries entries <- either fail pure $ eitherDecode (responseBody resp)
  -- for each snapshot, call endpoint telling GHC version
  ghcVersions <- forM entries $ \(_ltsMajor, ltsFull) -> do
    let req2 = ghcVersionEndpoint ltsFull
    resp <- httpLbs req2 gConnPool <&> responseBody
    ghcVer <- maybe (fail $ "non-utf8 response " <> show resp) pure
            $ decode ("\"" <> resp <> "\"") -- reuse aeson to avoid a dependency for utf8
    pure (ltsFull, ghcVer)
  -- print out
  forM_ ghcVersions $ \(lts, ghc) -> putStrLn $
    ghc <> ": " <> lts
