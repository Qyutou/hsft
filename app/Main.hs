{-|
Module      : Main
Description : Module start the application
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com

Simple system fetch tool written on haskell. 
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Render ( render )
import Config ( config )

main :: IO ()
main = do
    render config
