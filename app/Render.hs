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
import Data.List ( maximumBy, isInfixOf )
import Data.Ord ( comparing )

import Fetch ( fetchData )
import Colorizer ( applyFetchColors, colorizeText )
import DataTypes ( FetchFormat(..), getLength, Colors(..) )
import Config ( separator
              , colors )


-- | Add spaces to make line have certain length
addSpaces :: FetchFormat -- ^ The fetchedData
          -> Int -- ^ Required new length
          -> FetchFormat
addSpaces f l = f { sep = T.concat [ sep f
                                   , T.replicate (l - getLength f) " "] } 

-- | Put the text between two borders
addBorder :: T.Text -- ^ Text
          -> Int -- ^ Color
          -> T.Text -- ^ Color
          -> T.Text -- ^ Colored border on the begin and end of the text
addBorder t l c
    | "line" `isInfixOf` T.unpack t = T.concat [colorizeText "├" c
                                               , colorizeText (T.replicate l "─") c
                                               , colorizeText "┤" c, "\n"]
    | otherwise =  T.concat [colorizeText "│ " c, t, colorizeText " │" c, "\n"]

-- | Add colored border to the text
addBorders :: [T.Text] -- ^ Text
           -> Colors   -- ^ Colors
           -> Int      -- ^ Length of the borders, must be 2 symbols longer than longest string
           -> [T.Text] -- ^ Text with borders
addBorders t c l = [ colorizeText (T.concat [ "┌", T.replicate l "─", "┐" ]) (borderColor c), "\n"
                   , T.concat $ map (\x -> addBorder x l (borderColor c)) t 
                   , colorizeText (T.concat [ "└", T.replicate l "─", "┘" ]) (borderColor c) ] 

-- | Parse the fetchData
parseFetchData :: [FetchFormat] -> IO T.Text
parseFetchData fetch = do
    -- Find the length of the longest string
    let maximumLength = getLength $ maximumBy (comparing getLength) fetch :: Int

    -- Add spaces to every line after separator
    -- It makes every line to have the same length
    let fetchWithSpaces = map (`addSpaces` maximumLength) fetch

    -- Apply colors to the fetched data
    let coloredFetchData = map (`applyFetchColors` colors) fetchWithSpaces 
    
    -- Convert FetchFormat type to [Text]
    let fetchDataText = map (T.pack . show) coloredFetchData :: [T.Text]
    
    -- Add colored borders to the text
    let textWithBorders = addBorders fetchDataText colors (maximumLength + 2)

    -- Concat all text lines
    let result = T.concat textWithBorders
    
    return result 

-- | Print text with utf8 encoding
printLnText :: T.Text -> IO ()
printLnText t = BSC.putStrLn $ E.encodeUtf8 t

-- | Write a data based on config
render :: T.Text -> IO ()
render conf = do
    fetchedData <- fetchData (T.words conf) []
    parsedData <- parseFetchData fetchedData
    printLnText parsedData
