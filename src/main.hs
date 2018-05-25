{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude         hiding (id)
import           Hakyll

main :: IO ()
main = hakyllWith defaultConfiguration $ do

    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Static directories
    match ("images/*" .||. "font/**/*" .||. "_redirects") $ do
        route   idRoute
        compile copyFileCompiler

    -- Render Index
    create ["index.html"] $ do
      route idRoute
      compile $ do
        makeItem ""
          >>= loadAndApplyTemplate "templates/index.html" defaultContext
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    -- Read templates
    match "templates/*" $ compile templateCompiler
