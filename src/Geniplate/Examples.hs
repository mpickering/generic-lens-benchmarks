{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Geniplate.Examples where

import Auxiliary.Tree (Tree(..))
import qualified Auxiliary.Logic as A
import Auxiliary.Logic (Logic(..))
import Auxiliary.WTree
import Auxiliary.FlatProd

import Language.Haskell.Parser
import Language.Haskell.Syntax
import GHC.Real (Ratio((:%)))
import Language.Haskell.Syntax
import Control.Monad.State.Strict

import Data.Generics
import Data.Generics.Geniplate
import Data.Generics.Uniplate.Data hiding (transformBi, transformBiM, universeBi)

updateFlatProdInt :: (Int -> Int) -> (FlatProd -> FlatProd)
updateFlatProdInt = $(genTransformBi' [t| (Int -> Int) -> (FlatProd -> FlatProd)  |])

selectFlatProdInt :: FlatProd -> [Int]
selectFlatProdInt = $(genUniverseBi' [t| FlatProd -> [Int] |])

mapTree :: (Int -> Int) -> (Tree Int) -> (Tree Int)
mapTree =
  $(genTransformBi' [t| (Int -> Int) -> Tree Int -> Tree Int |] )

mapTreeInc = mapTree (+1)

getUnique :: Int -> State Int Int
getUnique _ = do
    u <- get
    modify (+1)
    return u

renumberIntT :: Tree Int -> State Int (Tree Int)
renumberIntT =
  $(genTransformBiM' [t| (Int -> State Int Int) -> (Tree Int -> State Int (Tree Int)) |])  getUnique

renumberIntL :: Logic -> State Int Logic
renumberIntL =
  $(genTransformBiM' [t| (Int -> State Int Int) -> (Logic -> State Int Logic) |])  getUnique

renumberTree :: Tree Int -> Tree Int
renumberTree l = evalState (renumberIntT l) 0

renumberLogic :: Logic -> Logic
renumberLogic l = evalState (renumberIntL l) 0

rmWeights :: WTree Int Int -> WTree Int Int
rmWeights = $(genTransformBi' [t| (WTree Int Int -> WTree Int Int)
                                  -> (WTree Int Int -> WTree Int Int) |]) rmAdhoc
  where
    rmAdhoc :: WTree Int Int -> WTree Int Int
    rmAdhoc (WithWeight t w) = t -- Constructor ad-hoc case
    rmAdhoc t                = t -- Generic case (no need to handle, everywhere takes care of it)
-----------------------

selectDumb :: WTree Int Int -> [Int]
selectDumb t = $(genUniverseBi' [t| (WTree Int Int -> [Int]) |]) t

--selectSmart :: WTree Int Int -> [Int]
--selectSmart t = selectSYBEverythingQr t

--------------------------------
updateStringLogic :: (Char -> Char) -> Logic -> Logic
updateStringLogic f = $(genTransformBi' [t| (Char -> Char) -> Logic -> Logic |] ) f

updateStringHsModule :: (Char -> Char) -> HsModule -> HsModule
updateStringHsModule f = $(genTransformBi' [t| (Char -> Char) -> (HsModule -> HsModule) |]) f

updateLogicConst :: Logic -> Logic
updateLogicConst l = updateStringLogic (const 'y') l

updateHsModuleConst :: HsModule -> HsModule
updateHsModuleConst hs = updateStringHsModule (const 'y') hs

updateHsAssocHsModule :: (HsAssoc -> HsAssoc) -> HsModule -> HsModule
updateHsAssocHsModule f = $(genTransformBi' [t| (HsAssoc -> HsAssoc) -> (HsModule -> HsModule) |])f

setHsModAssoc :: HsModule -> HsModule
setHsModAssoc = updateHsAssocHsModule (const HsAssocNone)
