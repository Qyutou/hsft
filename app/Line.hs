{-|
Module      : Fetch
Description : Module which simply used as line
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com
-}

module Line where

import qualified Data.Text as T

-- | It just return the IO "line"
line :: IO T.Text
line = return $ T.pack "line"
