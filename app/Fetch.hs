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

import DataTypes ( FetchField (..), TableField(..) )

import Config ( separator, fetchFields )

-- | This method is used to get a single fetch result by the name and the function
fetch :: FetchField     -- ^ Field
      -> IO TableField -- ^ Return field of the table
fetch f = do
    val <- snd $ field f :: IO T.Text 
    return $ TableFetchValue (fst $ field f, separator, val)

-- | Search for fetch command in the list of fetch fields
findFetchCommand :: T.Text          -- ^ Title
                 -> [FetchField]    -- ^ List of fields
                 -> Maybe (IO T.Text) -- ^ Return the fetch method
findFetchCommand title fields = lookup title $ map field fields

-- | This method return a table field based on the title
getValue :: T.Text        -- ^ The title
         -> [FetchField]  -- ^ List of all title-function values 
         -> IO TableField -- ^ Return table field
getValue "line" _ = return TableLine
getValue "emptyLine" _ = return TableEmptyLine
getValue title fields = do
    let fetchCommand = findFetchCommand title fields
    case fetchCommand of
        Just cmd -> do
            fetch $ FetchField (title, cmd)
        Nothing -> return TableError


-- | This method is used to get a list of table fields from configuration
getTable :: T.Text             -- ^ This is the configuration
            -> IO [TableField] -- ^ Return a list of results
getTable config = do
    let configWords = T.words config

    mapM (`getValue` fetchFields) configWords :: IO [TableField]
