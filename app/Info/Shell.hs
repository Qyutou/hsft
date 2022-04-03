{-|
Module      : Info.Shell
Description : Module which fetch current shell
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Info.Shell where

import qualified Data.Text as T

import System.Environment.Blank ( getEnv )

-- | Fetch currently used shell
-- | It simply get the value of environmental variable SHELL
fetchShell :: IO T.Text
fetchShell = do
    shell <- getEnv "SHELL"
    case shell of
        Just s -> return $ T.pack s
        Nothing -> return "not found"
