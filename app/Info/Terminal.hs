{-|
Module      : Info.Terminal
Description : Module which fetch current terminal
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Info.Terminal where

import qualified Data.Text as T

import Text.RegexPR ( getbrsRegexPR )
import System.Environment.Blank ( getEnv )

-- | Fetch current terminal emulator name
fetchTerminal :: IO T.Text
fetchTerminal = do
    terminal <- getEnv "TERM"
    -- Check if TERM variable exist
    case terminal of
        Nothing -> return "Not Found"
        Just terminal -> do
            let values = getbrsRegexPR "(?<=xterm-).*" terminal
            -- Check if the terminal name were found
            if null values
                then 
                    return "Linux"
                else
                    return $ T.pack $ head values
