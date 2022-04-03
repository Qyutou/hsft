{-|
Module      : Info.Kernel
Description : Module which fetch system kernel
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Info.Kernel where

import qualified Data.Text as T

import Text.RegexPR ( getbrsRegexPR )

-- | Fetch curren kernel version
-- | This function reads file /proc/version and find the kernel version
fetchKernel :: IO T.Text
fetchKernel = do
    -- Read contents of the file with current kernel version
    contents <- readFile "/proc/version"

    -- Find the 'version [here the version] ' pattern
    let kernel = T.pack $ head $ getbrsRegexPR "(?<=version\\s)(.*?)(?=\\s)" contents :: T.Text

    return kernel
