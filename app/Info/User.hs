{-|
Module      : Info.User
Description : Module which is used to get current user name
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Info.User
    ( fetchUser
    ) where

import qualified Data.Text as T

import System.Environment.Blank ( getEnv )

-- | Get name of current user
fetchUser :: IO T.Text
fetchUser = do
    shell <- getEnv "USER"
    case shell of
        Just s -> return $ T.pack s
        Nothing -> return "not found"
