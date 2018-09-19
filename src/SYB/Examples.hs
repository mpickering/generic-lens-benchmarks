{-# LANGUAGE RankNTypes #-}
module SYB.Examples where

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

updateFlatProdInt :: (Int -> Int) -> FlatProd -> FlatProd
updateFlatProdInt f = everywhere (mkT f)

selectFlatProdInt :: FlatProd -> [Int]
selectFlatProdInt = selectInt everything

eqTree :: WTree Int Int -> Bool
eqTree t = t `geq` t

mapTree :: (Int -> Int) -> (Tree Int -> Tree Int)
mapTree f = everywhere (mkT f)

mapTreeInc :: Tree Int -> Tree Int
mapTreeInc = mapTree (+1)

getUnique :: Int -> State Int Int
getUnique _ = do
    u <- get
    modify (+1)
    return u

renumberInt :: Data a => a -> State Int a
renumberInt = everywhereM (mkM getUnique)

renumberTree :: Tree Int -> Tree Int
renumberTree l = evalState (renumberInt l) 0

renumberLogic :: Logic -> Logic
renumberLogic l = evalState (renumberInt l) 0

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

-----------------------


selectInt :: Data a => (forall r. (r -> r -> r) -> GenericQ r -> GenericQ r) -> a -> [Int]
selectInt ething = ething (++) (mkQ [] f) where
  f :: Int -> [Int]
  f x = [x]

everythingR :: (r -> r -> r) -> GenericQ r -> GenericQ r
everythingR k f x = foldr k (f x) (gmapQ (everythingR k f) x)

everythingQl :: (r -> r -> r) -> GenericQ r -> GenericQ r
everythingQl k f x = gmapQl k (f x) (everythingQl k f) x

everythingQr :: (r -> r -> r) -> GenericQ r -> GenericQ r
everythingQr k f x = gmapQr k (f x) (everythingQr k f) x

selectInt_acc :: Data a => a -> [Int]
-- selectInt_acc x = everything (.) (mkQ id (:)) x []
selectInt_acc x = everythingAcc (mkQ id (:)) x []

everythingAcc :: GenericQ (r -> r) -> GenericQ (r -> r)
everythingAcc f x acc =
  gmapQr ($) (f x acc) (everythingAcc f) x

{-
r' = r -> r
gmapQr :: forall r r'. (r' -> r -> r) -> r -> (forall d. Data d => d -> r') -> a -> r
  gmapQr o r0 f x0 = unQr (gfoldl k (const (Qr id)) x0) r0
    where
      k :: Data d => Qr r (d->b) -> d -> Qr r b
      k (Qr c) x = Qr (\r -> c (f x `o` r))
-}

selectSYBEverything :: WTree Int Int -> [Int]
selectSYBEverything t = (selectInt everything t)

selectSYBEverythingR :: WTree Int Int -> [Int]
selectSYBEverythingR t = selectInt everythingR t

selectSYBEverythingQl :: WTree Int Int -> [Int]
selectSYBEverythingQl t = selectInt everythingQl t

selectSYBEverythingQr :: WTree Int Int -> [Int]
selectSYBEverythingQr t = selectInt everythingQr t

selectDumb :: WTree Int Int -> [Int]
selectDumb t = selectSYBEverything t

selectSmart :: WTree Int Int -> [Int]
selectSmart t = selectSYBEverythingQr t

--------------------------------
updateInt :: (Data a) => a -> a
updateInt = everywhere (mkT (\n -> if odd n then n+1 else (n-1 :: Int)))

updateStringLogic :: (Char -> Char) -> Logic -> Logic
updateStringLogic f = everywhere (mkT f)

updateStringHsModule :: (Char -> Char) -> HsModule -> HsModule
updateStringHsModule f = everywhere (mkT f)

updateLogicConst :: Logic -> Logic
updateLogicConst l = updateStringLogic (const 'y') l

updateHsModuleConst :: HsModule -> HsModule
updateHsModuleConst hs = updateStringHsModule (const 'y') hs
