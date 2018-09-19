{-# LANGUAGE DeriveGeneric #-}
module Auxiliary.MutRec where

import GHC.Generics


data Mut a = Weight Int (Mut a) | Mut (Rec a) (Rec a) deriving Generic

data Rec a = Leaf a | Rec (Mut a) deriving Generic
