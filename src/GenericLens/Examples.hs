{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
-- Like this for the HsModule example, would be fine with INLINE pragmas on
-- the generic methods
module GenericLens.Examples where

import Auxiliary.Tree (Tree(..))
import qualified Auxiliary.Logic as A
import Auxiliary.Logic (Logic(..))
import Auxiliary.WTree
import Auxiliary.HsModule
import Auxiliary.FlatProd
import Auxiliary.MutRec

import Control.Monad.State.Strict

import Data.Generics.Product
import Data.Generics.Product.Constraints
import Data.Generics (Data, mkT)
import Data.DList

import GHC.Generics

import Language.Haskell.Syntax

import Control.Lens
import GHC.Real

updateFlatProdInt :: (Int -> Int) -> FlatProd -> FlatProd
updateFlatProdInt f = over (types @Int) f

selectFlatProdInt :: FlatProd -> [Int]
selectFlatProdInt = views (types @Int) (:[])


mapTree :: (Int -> Int) -> (Tree Int -> Tree Int)
mapTree f = over (types @Int) f

mapTreeInc :: Tree Int -> Tree Int
mapTreeInc = mapTree (+1)

getUnique :: Int -> State Int Int
getUnique _ = do
    u <- get
    modify (+1)
    return u

--renumberInt :: (Generic a,0i--0i => a -> State Int a
renumberInt l = (types @Int) getUnique l

renumberTree :: Tree Int -> Tree Int
renumberTree l = evalState (renumberInt l) 0

renumberLogic :: Logic -> Logic
renumberLogic l = evalState (renumberInt l) 0

rmAdhoc :: WTree Int Int -> WTree Int Int
rmAdhoc (WithWeight t w) = t -- Constructor ad-hoc case
rmAdhoc t                = t -- Generic case (no need to handle, everywhere takes care of it)

rmWeights :: WTree Int Int -> WTree Int Int
rmWeights = rmAdhoc . over (types @(WTree Int Int)) rmWeights

rmAdHoc2 :: Mut a -> Mut a
rmAdHoc2 (Weight _ m) = m
rmAdHoc2 m = m

rmMutWeights :: Mut Int -> Mut Int
rmMutWeights = rmAdHoc2 . over (types @(Mut Int)) rmMutWeights

-----------------------


selectInt :: WTree Int Int -> [Int]
selectInt = toListOf (types @Int)

--selectIntDList :: _ -> [Int]
selectIntDList d = toList $ views (types @Int) singleton d



selectDumb :: WTree Int Int -> [Int]
selectDumb t = selectInt t

selectSmart :: WTree Int Int -> [Int]
selectSmart t = selectIntDList t

--------------------------------

updateStringLogic :: (Char -> Char) -> Logic -> Logic
updateStringLogic f = over types f

--Something goes wrong as the AST contains an Integer


updateStringHsModule :: (Char -> Char) -> HsModule -> HsModule
updateStringHsModule f = over types f
--


updateLogicConst :: Logic -> Logic
updateLogicConst l = updateStringLogic (const 'y') l

updateHsModuleConst :: HsModule -> HsModule
updateHsModuleConst hs = updateStringHsModule (const 'y') hs

updateHsAssocHsModule :: (HsAssoc -> HsAssoc) -> HsModule -> HsModule
updateHsAssocHsModule f = over types f

setHsModAssoc :: HsModule -> HsModule
setHsModAssoc = updateHsAssocHsModule (const HsAssocNone)

