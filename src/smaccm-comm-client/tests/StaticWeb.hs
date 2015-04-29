{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Snap.Core (Snap, route)
import qualified Snap.Http.Server as HTTP
import           Snap.Util.FileServe (serveDirectory)

main :: IO ()
main = HTTP.simpleHttpServe snapCfg body
  where
  body = route [ ("", serveDirectory "./web/") ]

  snapCfg :: HTTP.Config Snap ()
  snapCfg = HTTP.setPort 8080 HTTP.defaultConfig

