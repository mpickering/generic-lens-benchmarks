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
module OneLiner.Examples where



import Auxiliary.Tree (Tree(..))
import qualified Auxiliary.Logic as A
import Auxiliary.Logic (Logic(..))
import Auxiliary.WTree
import Auxiliary.HsModule
import Auxiliary.FlatProd

import Language.Haskell.Parser
import Language.Haskell.Syntax
import Control.Monad.State.Strict

import Data.Generics

import Generics.OneLiner
import Data.Type.Equality

import Data.Functor.Identity
import Data.Monoid
import Control.Applicative
import qualified Data.DList as D

-- Copied from the examples directory

class TinplateHelper (p :: Bool) a b where
  trav' :: Applicative f => (a -> f a) -> b -> f b

instance TinplateHelper 'True a a where trav' f = f

instance {-# OVERLAPPABLE #-} (ADT b, Constraints b (TinplateAlias a)) => TinplateHelper 'False a b where
  trav' = tinplate

instance TinplateHelper 'False a Char where trav' _ = pure
instance TinplateHelper 'False a Double where trav' _ = pure
instance TinplateHelper 'False a Float where trav' _ = pure
instance TinplateHelper 'False a Int where trav' _ = pure
instance TinplateHelper 'False a Word where trav' _ = pure
instance TinplateHelper 'False a Integer where trav' _ = pure

class TinplateAlias a b where
  trav :: Applicative f => (a -> f a) -> b -> f b
instance TinplateHelper (a == b) a b => TinplateAlias a b where
  trav = trav' @(a == b)


tinplate :: forall a b f. (ADT b, Constraints b (TinplateAlias a), Applicative f) => (a -> f a) -> b -> f b
tinplate f = gtraverse @(TinplateAlias a) (trav f)


--tinselect :: (ADT b, ConstrainMonoid m => (a -> m) -> m
tinselect f = getConst . tinplate (Const . f)

tinmap f = runIdentity . tinplate (Identity . f)




test1, test2 :: [[(Char, Int)]] -> [[(Char, Int)]]
test1 = runIdentity . tinplate (Identity . f) where
  f :: Char -> Char
  f = succ
test2 = runIdentity . tinplate (Identity . f) where
  f :: Int -> Int
  f = succ

test12 :: [[(Char, Int)]]
test12 = test1 as ++ test2 as where as = [[('x', 1)], [('y', 2)]]

-------------------


updateFlatProdInt :: (Int -> Int) -> FlatProd -> FlatProd
updateFlatProdInt f = tinmap f

selectFlatProdInt :: FlatProd -> [Int]
selectFlatProdInt = tinselect (:[])

--------------------

class MyEq a where
  myEq :: a -> a -> All

instance MyEq Int where
  myEq s t = All (s == t)

instance (MyEq a, MyEq b) => MyEq (WTree a b) where
  myEq s t =
    mzipWith' @MyEq (myEq (ctorIndex s) (ctorIndex t)) myEq s t

eqTree :: WTree Int Int -> Bool
eqTree t = getAll $ t `myEq` t

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
renumberInt = tinplate getUnique

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

updateHsAssocHsModule :: (HsAssoc -> HsAssoc) -> HsModule -> HsModule
updateHsAssocHsModule f = tinmap f

setHsModAssoc :: HsModule -> HsModule
setHsModAssoc = updateHsAssocHsModule (const HsAssocNone)
