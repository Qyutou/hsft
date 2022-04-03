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

-- | Data type with fetched data, it is created to prevent parsing this data multiple times
data FetchFormat = FetchFormat
    { title :: T.Text
    , sep   :: T.Text
    , info  :: T.Text 
    } deriving (Eq)

instance Show FetchFormat where
    show f = T.unpack $ T.concat [title f, sep f, info f] 

-- | Method to get length of the fetch line
getLength :: FetchFormat -> Int
getLength f = T.length $ T.concat [title f, sep f, info f]

-- | This newtype is used to describe the title and method
newtype FetchField = FetchField 
    { field :: (T.Text, IO T.Text) }
