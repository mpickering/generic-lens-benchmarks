{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}
module Auxiliary.FlatProd where

import GHC.Generics
import Data.Data
import Control.DeepSeq

data FlatProd =
  FlatProd Int Char Bool Int Char Bool String String String String
  deriving (Generic, Data, Eq, Ord, Show, NFData)
