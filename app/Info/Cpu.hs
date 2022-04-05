{-|
Module      : Info.Cpu
Description : Module which gets current cpu
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Info.Cpu where

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B

import Data.Char (isSpace)

-- | Find current cpu name from contents of the file
parseContents :: [T.Text] -> Maybe T.Text
parseContents [] = Nothing
parseContents (x:xs) = 
    if "model name" `T.isInfixOf` x
        then
            Just $ T.dropWhile isSpace . T.takeWhileEnd (/= ':') $ x
        else
            parseContents xs

-- | Get current cpu
fetchCpu :: IO T.Text 
fetchCpu = do
    -- Read the /proc/cpuinfo
    contents <- B.readFile "/proc/cpuinfo"

    -- Get the cpu name from the contents
    let cpu = parseContents $ T.lines $ E.decodeUtf8 contents 

    -- Check if cpu name were found
    case cpu of
        Just cpu -> return cpu
        Nothing -> return "not found"
