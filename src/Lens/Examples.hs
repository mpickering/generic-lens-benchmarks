{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE
  DataKinds,
  TypeFamilies,
  TypeOperators,
  FlexibleContexts,
  TypeApplications,
  FlexibleInstances,
  AllowAmbiguousTypes,
  ScopedTypeVariables,
  UndecidableInstances,
  MultiParamTypeClasses,
  NoMonomorphismRestriction
#-}
module Lens.Examples where



import Auxiliary.Tree (Tree(..))
import qualified Auxiliary.Logic as A
import Auxiliary.Logic (Logic(..))
import Auxiliary.WTree
import Auxiliary.HsModule
import Auxiliary.FlatProd

import Language.Haskell.Parser
import Language.Haskell.Syntax
import Language.Haskell.Syntax
import Control.Monad.State.Strict

import Data.Generics

import Data.Data.Lens
import Data.Type.Equality

import Data.Functor.Identity
import Data.Monoid
import Control.Applicative
import qualified Data.DList as D

-- Copied from the examples directory


tinselect f = getConst . template (Const . f)

tinmap f = runIdentity . template (Identity . f)


updateFlatProdInt :: (Int -> Int) -> FlatProd -> FlatProd
updateFlatProdInt f = tinmap f

selectFlatProdInt :: FlatProd -> [Int]
selectFlatProdInt = tinselect (:[])


--------------------

mapTree :: (Int -> Int) -> (Tree Int -> Tree Int)
mapTree f = tinmap f

mapTreeInc :: Tree Int -> Tree Int
mapTreeInc = mapTree (+1)

getUnique :: Int -> State Int Int
getUnique _ = do
    u <- get
    modify (+1)
    return u

--renumberInt :: Generic a => a -> State Int a
renumberInt = template getUnique

renumberTree :: Tree Int -> Tree Int
renumberTree l = evalState (renumberInt l) 0

renumberLogic :: Logic -> Logic
renumberLogic l = evalState (renumberInt l) 0

rmWeights :: WTree Int Int -> WTree Int Int
rmWeights = tinmap rmAdhoc
  where
  -- It is a pity that an ad-hoc case for a type
  -- constructor is not easy to do in SYB
  -- (no mkT2!!)
  rmAdhoc :: WTree Int Int -> WTree Int Int
  rmAdhoc (WithWeight t w) = t -- Constructor ad-hoc case
  rmAdhoc t                = t -- Generic case (no need to handle, everywhere takes care of it)

-----------------------


selectDumb :: WTree Int Int -> [Int]
selectDumb t = tinselect singleton t
  where
    singleton x = [x]

selectSmart :: WTree Int Int -> [Int]
selectSmart t = D.toList $ tinselect singleton t
  where
    singleton x = D.singleton x

updateStringLogic :: (Char -> Char) -> Logic -> Logic
updateStringLogic f = tinmap f

updateStringHsModule :: (Char -> Char) -> HsModule -> HsModule
updateStringHsModule f = tinmap f

updateLogicConst :: Logic -> Logic
updateLogicConst l = updateStringLogic (const 'y') l

updateHsModuleConst :: HsModule -> HsModule
updateHsModuleConst hs = updateStringHsModule (const 'y') hs
