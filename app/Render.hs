{-|
Module      : Render
Description : Module which get data based on the config
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Render where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.Encoding as E
import qualified Data.Text as T

import Data.Char ( isSpace )
import Data.List ( maximumBy, isInfixOf, intercalate )
import Data.Ord ( comparing )

import Colorizer ( colorizeText )
import DataTypes ( Colors(..)
                 , TableField(..)
                 , FieldAlign(..)
                 , getFieldLength
                 , getTitleLength
                 , getInfoLength )
import Config ( separator
              , colors
              , fieldAlign )
import Fetch ( getTable )

-- | This method is used to add spaces to the every field,
-- | to make all fields have the same length
alignTableField :: TableField -> FieldAlign -> Int -> Int -> Int-> TableField
alignTableField field align maxLen maxTitleLen maxInfoLen = case field of
    TableLine      -> TableLine
    TableError     -> TableError
    TableEmptyLine -> TableEmptyLine
    TableFetchValue (t, s, i) -> case align of
        NoAlign   -> do
            let amount = maxLen - getFieldLength field
            TableFetchValue (t, s, T.concat [i, T.replicate amount " "])
        Separator -> do
            let amount = maxLen - getFieldLength field
            TableFetchValue (t, T.concat [s, T.replicate amount " "], i)
        Title     -> do
            let amountTitle = maxTitleLen - T.length t
            let amountOnEnd = (maxInfoLen + maxTitleLen + T.length separator) - amountTitle - getFieldLength field
            TableFetchValue ( T.concat [ t, T.replicate amountTitle " "]
                            , s
                            , T.concat [ i, T.replicate amountOnEnd " "] )

-- | This method is used to apply colors to table field
applyColor' :: TableField -> Colors -> TableField
applyColor' field colors = case field of
    TableLine                 -> TableLine
    TableError                -> TableError
    TableEmptyLine            -> TableEmptyLine
    TableFetchValue (t, s, i) -> TableFetchValue ( colorizeText t $ titleColor colors
                                                 , colorizeText s $ separatorColor colors
                                                 , colorizeText i $ infoColor colors)

-- | Text which is used as a line
lineBorder :: T.Text -- ^ Border color
           -> Int    -- ^ Length 
           -> T.Text -- ^ Border 
lineBorder c l = T.concat [ colorizeText "├" c
                          , colorizeText (T.replicate l "─") (borderColor colors)
                          , colorizeText "┤" c
                          , "\n"]

-- | Text which is used as an empty line
emptyLineBorder :: T.Text -- ^ Border color
                -> Int    -- ^ Length - amount of spaces
                -> T.Text -- ^ Certain amount of spaces between borders
emptyLineBorder c l = T.concat [ colorizeText "│" c
                               , T.replicate l " " 
                               , colorizeText "│" c
                               , "\n"]

-- | Text which is used as border
fieldBorder :: T.Text -- ^ Text
            -> T.Text -- ^ Border color
            -> T.Text -- ^ Text between colored borders
fieldBorder t c = T.concat [ colorizeText "│ " c
                           , t
                           , colorizeText " │" c
                           , "\n"]



-- | Add border to the table field, based on field's type
applyBorder' :: TableField -> Colors -> Int -> T.Text
applyBorder' field colors len = case field of
    TableLine -> lineBorder (borderColor colors) (len + 2)
    TableEmptyLine -> emptyLineBorder (borderColor colors) (len + 2)
    TableFetchValue (t, s, i) -> fieldBorder (T.concat [t, s, i]) (borderColor colors)
    TableError -> ""

-- | This method is used to add borders to the table
applyBorder :: [TableField] -> Int -> Colors -> T.Text
applyBorder table len colors = T.concat [ colorizeText (T.concat [ "┌", T.replicate (len + 2) "─", "┐" ]) (borderColor colors), "\n"
                                        , T.concat $ map (\x -> applyBorder' x colors len) table
                                        , colorizeText (T.concat [ "└", T.replicate (len + 2) "─", "┘" ]) (borderColor colors) ] 

-- | This method is used to parse the table
parseTable :: [TableField] -> IO T.Text
parseTable table = do
    let maximumLength = getFieldLength $ maximumBy (comparing getFieldLength) table
    let maximumTitleLength = getTitleLength $ maximumBy (comparing getTitleLength) table
    let maximumInfoLength = getInfoLength  $ maximumBy (comparing getInfoLength) table

    let alignedTable = map (\x -> alignTableField x fieldAlign maximumLength maximumTitleLength maximumInfoLength) table
    let coloredTable = map (`applyColor'` colors) alignedTable

    let newMaxLen = getFieldLength $ maximumBy (comparing getFieldLength) alignedTable
    let tableWithBorders = applyBorder coloredTable newMaxLen colors

    return tableWithBorders

-- | Print text with UTF-8 encoding
printLnText :: T.Text -> IO ()
printLnText t = BSC.putStrLn $ E.encodeUtf8 t

-- | Write a data based on configuration
render :: T.Text -> IO ()
render conf = do
    table <- getTable conf
    result <- parseTable table
    printLnText result
