{-|
Module      : Fetch
Description : Module which get data based on the config
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import qualified Data.Text as T

import DataTypes ( FetchFormat (..), FetchField (..) )

import Config ( separator, fetchFields )


-- | This method is used to get a single fetch result by the name and the function
fetch :: FetchField     -- ^ Field
      -> IO FetchFormat -- ^ Return fetchFormat
fetch f = do
    val <- snd $ field f :: IO T.Text 
    return FetchFormat {title = fst $ field f, sep = separator, info = val}

-- | Search for fetch command in the list of fetch fields
findFetchCommand :: T.Text          -- ^ Title
                 -> [FetchField]    -- ^ List of fields
                 -> Maybe (IO T.Text) -- ^ Return the fetch method
findFetchCommand title fields = lookup title $ map field fields

-- | Get all requred data based on config
fetchData :: [T.Text]         -- ^ Config as a list of words
           -> [FetchFormat]    -- ^ Initial value
           -> IO [FetchFormat] -- ^ Return all required fetched data
fetchData [] val = return val
fetchData (x:xs) val = do
    let fetchCommand = findFetchCommand x fetchFields

    case fetchCommand of
        Just com -> do
            msg <- fetch $ FetchField (x, com)
            fetchData xs (val ++ [msg])
        Nothing -> fetchData xs val
