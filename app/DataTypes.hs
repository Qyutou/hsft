{-|
Module      : DataTypes
Description : Module to create data types
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com
-}

module DataTypes where

import qualified Data.Text as T

-- | Data Type with color of all elements
data Colors = Colors
    { borderColor    :: T.Text
    , titleColor     :: T.Text
    , separatorColor :: T.Text
    , infoColor      :: T.Text 
    }

-- | This data type is used to define the alignment 
data FieldAlign = NoAlign | Separator | Title

-- | This newtype is used to describe the title and method
newtype FetchField = FetchField 
    { field :: (T.Text, IO T.Text) }

-- | This data type is used to store fetched results
data TableField = TableLine
                | TableError
                | TableEmptyLine
                | TableFetchValue (T.Text, T.Text, T.Text)

-- | Add show instance for the table fields
instance Show TableField where
    show f = case f of
        TableLine      -> "line"
        TableEmptyLine -> "nothing"
        TableError     -> ""
        TableFetchValue (title, sep, info) -> T.unpack $ T.concat [title, sep, info]

getFieldLength :: TableField -> Int
getFieldLength field = case field of 
    TableFetchValue (t, s, i) -> T.length $ T.concat [t, s, i]
    _ -> 0

getTitleLength :: TableField -> Int
getTitleLength field = case field of
    TableFetchValue (t, _, _) -> T.length t
    _ -> 0

getInfoLength :: TableField -> Int
getInfoLength field = case field of
    TableFetchValue (_, _, i) -> T.length i
    _ -> 0
