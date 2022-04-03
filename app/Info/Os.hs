{-|
Module      : Info.Os
Description : Module which fetch current distro
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Info.Os where

import qualified Data.Text as T

import Data.Text ( isInfixOf )
import Text.RegexPR ( getbrsRegexPR )

-- | Fetch the os name (distro)
-- | It reads /etc/os-release file and take the distro name
fetchOs :: IO T.Text
fetchOs = do
    -- Read contents from the file
    contents <- readFile "/etc/os-release"
    
    -- Find os name using regex
    let osName = findOsName $ lines contents

    return osName

-- | Find os name
-- | It searches for the line started with "PRETTY_NAME", and return text in the quotes
findOsName :: [String] -> T.Text
findOsName [] = "Not found"
findOsName (x:xs) = 
    if "PRETTY_NAME" `isInfixOf` T.pack x
        then
            T.pack $ head $ getbrsRegexPR "(?<=\")(.*)(?=\")" x
        else
            findOsName xs
