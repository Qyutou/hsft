{-|
Module      : Info.Editor
Description : Module which fetch current editor specifed in $EDITOR
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com

This module may be used as a template to fetch any value based on the environmental variable
-}

{-# LANGUAGE OverloadedStrings #-}

module Info.Editor where

import qualified Data.Text as T

import System.Environment.Blank ( getEnv )

-- | Fetch currently used editor
-- | It simply get the value of environmental variable $EDITOR
fetchEditor :: IO T.Text
fetchEditor = do
    -- Get value of $EDITOR environmental variable
    shell <- getEnv "EDITOR"

    -- Check if the environmental variable exists
    case shell of
        Just s -> return $ T.pack s
        Nothing -> return "not found"
