{-|
Module      : Info.DiskUsage
Description : Module which gets current disk usage
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com

This module is slow, it based on df command.
Maybe the speed will be increased if rewrite this in pure haskell.
-}

{-# LANGUAGE OverloadedStrings #-}

module Info.DiskUsage 
    ( fetchDiskUsage
    , fetchAvailSpace
    ) where

import qualified Data.Text as T

import Info.Cmd (fetchCmd)
import System.Directory (doesDirectoryExist)

-- | Get current disk usage in format 'used/total' of certain dir
fetchDiskUsage :: FilePath  -- ^ Path to directory
               -> IO T.Text -- ^ Disk usage
fetchDiskUsage p = do
    isPathCorrent <- doesDirectoryExist p
    if isPathCorrent
        then do
            dfResult <- fetchCmd "df" ["-h", T.pack p] 
            let dfWords = reverse . T.words $ dfResult
            if length dfWords > 5
                then do
                    return $ T.concat [dfWords !! 3, "/", dfWords !! 4]
                else
                    return "not found"
        else
            return "not found"

-- | Get current available space on disk
fetchAvailSpace :: FilePath  -- ^ Path to directory
                -> IO T.Text -- ^ Disk usage
fetchAvailSpace p = do
    isPathCorrent <- doesDirectoryExist p
    if isPathCorrent
        then do
            dfResult <- fetchCmd "df" ["-h", T.pack p] 
            let dfWords = reverse . T.words $ dfResult
            if length dfWords > 3
                then do
                    return $ dfWords !! 2
                else
                    return "not found"
        else
            return "not found"


    
