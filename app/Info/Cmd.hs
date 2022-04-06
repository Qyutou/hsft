{-|
Module      : Info.Cmd
Description : Module which fetch any data by shell command
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com

This module is something like template for modules which handling shell commands
-}

{-# LANGUAGE OverloadedStrings #-}

module Info.Cmd where

import qualified Data.Text as T

import System.Process ( readProcessWithExitCode ) 
import System.Exit ( exitSuccess, exitFailure, ExitCode (..) )

-- | This method handles the output of running command
-- | In this case it just remove the newline
handleCommandOutput :: T.Text -> T.Text
handleCommandOutput t = T.concat $ T.lines t

-- | Fetch curren volume using pamixer
fetchCmd :: T.Text    -- ^ Command
                  -> [T.Text]  -- ^ Options
                  -> IO T.Text -- ^ Return command stdout
fetchCmd command options = do
    -- First argument is command
    -- Second argument is options
    -- Thirg arguments is stdinput
    -- This command return (exitcode, stdout, stderr)

    -- Execute the command
    x <- readProcessWithExitCode (T.unpack command)
                                 (map T.unpack options) 
                                 (T.unpack "")

    -- Check the exit code
    case x of
        -- If the command executed with ExitSuccess, return the output command
        (ExitSuccess, text, _) -> return $ handleCommandOutput $ T.pack text
        -- If the command executed with error, return the exit
        (ExitFailure x, _, _) -> return $ T.concat ["exit ", T.pack $ show x] 
