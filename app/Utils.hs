{-|
Module      : Utils
Description : Module which include some utility functions
Copyright   : (c) Alexey Seledkov, 2022
License     : MIT
Maintainer  : qyutou@gmail.com
-}

module Utils where

import qualified Data.Text as T

import DataTypes ( FetchField(..) )

-- | This functions is used to make the configuration look better
add :: T.Text     -- ^ Title of field
    -> IO T.Text  -- ^ Fetch command of this field
    -> FetchField -- ^ Return FetchField
add t f = FetchField { field = (t, f) }
