{-|
Module      : Info.Wm
Description : Module which fetch current wm
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Info.Wm where

import qualified Data.Text as T

import System.Environment.Blank ( getEnv )
import Data.Maybe ( isNothing )
import System.Process ( readProcess ) 
import Control.Monad ( when )
import Data.Text ( isInfixOf )
import Text.RegexPR ( getbrsRegexPR )
import Data.Char ( isSpace )

-- | Fetch curren wm
-- | It check if x is running, and extract the value from xprop
-- | This method is not too accurate and may not work with many wm
fetchWm :: IO T.Text
fetchWm = do
    desktop <- getEnv "DISPLAY"
    case desktop of
        Just _ -> readWm
        Nothing -> return "x not running"

-- | Find current WM
-- | I'm sure it works incorrect, but at least in my case (xmonad) it works.
readWm :: IO T.Text
readWm = do
     -- Run xprop with output like:
     -- _NET_SUPPORTING_WM_CHECK(WINDOW): window id # 0x400001
    strWithId <- readProcess "xprop" ["-root", "_NET_SUPPORTING_WM_CHECK"] ""
    
    -- Remove new line from the end
    let withoutNewLine = T.reverse . T.dropWhile (=='\n') . T.reverse $ T.pack strWithId :: T.Text
    
    -- Extract the id by remove everything before last space
    let id = T.takeWhileEnd (not . isSpace) withoutNewLine :: T.Text

    -- Run xprop with id, in output must be string like:
    -- _NET_WM_NAME = "xmonad"
    strWithWm <- readProcess "xprop" ["-id", "0x400001"] ""

    -- Find the wm name
    let wm = findWmName $ lines strWithWm :: T.Text

    return wm

-- | Find name of current wm from xprop -id output
findWmName :: [String] -> T.Text
findWmName [] = "not found"
findWmName (x:xs) = 
    if "_NET_WM_NAME" `isInfixOf` T.pack x
        then
            T.pack $ head $ getbrsRegexPR "(?<=\")(.*)(?=\")" x
        else
            findWmName xs
    
