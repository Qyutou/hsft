{-|
Module      : Info.Uptime
Description : Module which fetch system uptime
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Info.Uptime where

import qualified Data.Text as T

-- | Fetch current uptime
-- | This function reads file /proc/uptime and convert it to hours and minutes
fetchUptime :: IO T.Text
fetchUptime = do
    -- Read contents from /proc/uptime
    contents <- readFile "/proc/uptime"

    -- Get the value
    let unixMilis = read . head . words $ contents :: Float

    let hours = floor (unixMilis / 3600) :: Int
    let minutes = floor (unixMilis / 60) - (hours * 60) :: Int
    
    return $ T.concat [ T.pack . show $ hours
                      , "h "
                      , T.pack . show $ minutes
                      , "m"]
