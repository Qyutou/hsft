{-|
Module      : Info.Hostname
Description : Module which fetch current hostname
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Info.Hostname where

import qualified Data.Text as T

-- | Fetch current host name
fetchHostname :: IO T.Text
fetchHostname = do
    hostName <- readFile "/etc/hostname"
    return $ T.reverse . T.dropWhile (=='\n') . T.reverse $ T.pack hostName
