{-|
Module      : Colorizer
Description : Module which apply colors to the text
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Colorizer where

import qualified Data.Text as T

import DataTypes ( Colors(..) )

-- | Add the color to the text
colorizeText :: T.Text -- ^ Text
             -> T.Text -- ^ Color
             -> T.Text -- ^ Return colored text
colorizeText t "" = t
colorizeText t "black"   = colorizeText t "30" 
colorizeText t "red"     = colorizeText t "31" 
colorizeText t "green"   = colorizeText t "32" 
colorizeText t "yellow"  = colorizeText t "33" 
colorizeText t "blue"    = colorizeText t "34" 
colorizeText t "magenta" = colorizeText t "35" 
colorizeText t "cyan"    = colorizeText t "36" 
colorizeText t "white"   = colorizeText t "37" 
colorizeText t "brightBlack"   = colorizeText t "90" 
colorizeText t "brightRed"     = colorizeText t "91" 
colorizeText t "brightGreen"   = colorizeText t "92" 
colorizeText t "brightYellow"  = colorizeText t "93" 
colorizeText t "brightBlue"    = colorizeText t "94" 
colorizeText t "brigthMagenta" = colorizeText t "95" 
colorizeText t "brightCyan"    = colorizeText t "96" 
colorizeText t "brightWhite"   = colorizeText t "97" 
colorizeText text color = T.concat [ "\ESC[1;"
                                   , color
                                   , "m"
                                   , text
                                   , "\ESC[1;0m" ]
