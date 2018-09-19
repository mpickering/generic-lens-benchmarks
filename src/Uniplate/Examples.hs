{-# LANGUAGE RankNTypes #-}
module Uniplate.Examples where

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
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data

updateFlatProdInt :: (Int -> Int) -> (FlatProd -> FlatProd)
updateFlatProdInt f = transformBi f

selectFlatProdInt :: FlatProd -> [Int]
selectFlatProdInt = universeBi

mapTree :: (Int -> Int) -> (Tree Int -> Tree Int)
mapTree f = transformBi f

mapTreeInc :: Tree Int -> Tree Int
mapTreeInc = mapTree (+1)

getUnique :: Int -> State Int Int
getUnique _ = do
    u <- get
    modify (+1)
    return u

renumberInt :: Data a => a -> State Int a
renumberInt = transformBiM getUnique

renumberTree :: Tree Int -> Tree Int
renumberTree l = evalState (renumberInt l) 0

renumberLogic :: Logic -> Logic
renumberLogic l = evalState (renumberInt l) 0

{-
rmWeightsGen :: Data a => a -> a
rmWeightsGen = everywhere (mkT rmAdhoc)
  where
  -- It is a pity that an ad-hoc case for a type
  -- constructor is not easy to do in SYB
  -- (no mkT2!!)
  rmAdhoc :: WTree Int Int -> WTree Int Int
  rmAdhoc (WithWeight t w) = t -- Constructor ad-hoc case
  rmAdhoc t                = t -- Generic case (no need to handle, everywhere takes care of it)

rmWeights :: WTree Int Int -> WTree Int Int
rmWeights = rmWeightsGen
-}
-----------------------


selectDumb :: WTree Int Int -> [Int]
selectDumb t = universeBi t

--selectSmart :: WTree Int Int -> [Int]
--selectSmart t = selectSYBEverythingQr t

--------------------------------
updateInt :: (Data a) => a -> a
updateInt = transformBi ((\n -> if odd n then n+1 else (n-1 :: Int)))

updateStringLogic :: (Char -> Char) -> Logic -> Logic
updateStringLogic f = everywhere (mkT f)

updateStringHsModule :: (Char -> Char) -> HsModule -> HsModule
updateStringHsModule f = everywhere (mkT f)

updateLogicConst :: Logic -> Logic
updateLogicConst l = updateStringLogic (const 'y') l

updateHsModuleConst :: HsModule -> HsModule
updateHsModuleConst hs = updateStringHsModule (const 'y') hs
