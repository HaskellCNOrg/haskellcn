{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad(forM_)
import Prelude hiding (id)
import Control.Arrow ((>>>), (***), arr)
import Control.Category (id)
import Data.Monoid (mempty, mconcat)
import Text.Pandoc
import Hakyll

postsWildcardMatch = "blog/**/*"

main :: IO ()
main = hakyllWith config $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Static directories
    forM_ ["images/*"] $ \f -> match f $ do
        route   idRoute
        compile copyFileCompiler

    -- 
    match "about.html" $ route idRoute
    create "about.html" $ constA mempty
        >>> applyTemplateCompiler "templates/about.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler        
        
    match "study.md" $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
              >>> applyTemplateCompiler "templates/default.html"
              >>> relativizeUrlsCompiler
    
    -- Render posts
    match postsWildcardMatch $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Render Post Archive Page
    match "blog/allposts.html" $ route idRoute
    create "blog/allposts.html" $ constA mempty
        >>> arr (setField "title" "所有文章")
        >>> requireAllA postsWildcardMatch addPostList
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Render Blog Index
    match "blog/index.html" $ route idRoute
    create "blog/index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
        >>> requireAllA postsWildcardMatch (id *** arr (take 10 . reverse . chronological) >>> addPostList)
        >>> applyTemplateCompiler "templates/blog.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
    
    -- Tags
    create "tags" $
        requireAll postsWildcardMatch (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))


    -- Render  Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
--        >>> requireA "sidebar.md" (setFieldA "index" $ arr pageBody)        
        >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
        >>> requireAllA postsWildcardMatch (id *** arr (take 10 . reverse . chronological) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Render RSS feed
    match "rss.xml" $ route idRoute
    create "rss.xml" $
        requireAll_ postsWildcardMatch
            >>> mapCompiler (arr $ copyBodyToField "description")
            >>> renderRss feedConfiguration

    -- Read templates
    match "templates/*" $ compile templateCompiler
    
  where
    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = renderTagCloud tagIdentifier 100 120

    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"

    pageCompilerWithToc = pageCompilerWith defaultHakyllParserState withToc
    
    withToc = defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTemplate = "<h2 id=\"TOC\">TOC</h2>\n$toc$\n$body$"
        , writerStandalone = True
        }

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Haskell CN Community RSS feed."
    , feedDescription = "Haskell CN Community."
    , feedAuthorName  = "Haskell CN Community"
    , feedRoot        = "http://www.haskellcn.org"
    , feedAuthorEmail = "freizl@gmail.com"
    }

config :: HakyllConfiguration
config = defaultHakyllConfiguration
    { deployCommand = "rsync -c -r -ave 'ssh' \
                      \_site/* freizl_haskellcn@ssh.phx.nearlyfreespeech.net:/home/public"
    }
