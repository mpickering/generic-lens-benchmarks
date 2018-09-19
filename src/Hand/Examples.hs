module Hand.Examples where

import Auxiliary.Tree (Tree(..))
import qualified Auxiliary.Logic as A
import Auxiliary.Logic (Logic(..))
import Auxiliary.WTree

import Language.Haskell.Parser
import Language.Haskell.Syntax
import GHC.Real (Ratio((:%)))
import Language.Haskell.Syntax
import Control.Monad.State.Strict
import Auxiliary.FlatProd
import Control.Lens

updateFlatProdInt :: (Int -> Int) -> FlatProd -> FlatProd
updateFlatProdInt f (FlatProd a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
  = FlatProd (f a1) a2 a3 (f a4) a5 a6 a7 a8 a9 a10

traverseFlatProdInt :: Applicative f => (Int -> f Int) -> FlatProd -> f FlatProd
traverseFlatProdInt f (FlatProd a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
  = (\b1 b4 -> FlatProd b1 a2 a3 b4 a5 a6 a7 a8 a9 a10) <$> f a1 <*> f a4

updateFlatProdGen = over traverseFlatProdInt (+1)
selectFlatProdGen :: FlatProd -> [Int]
selectFlatProdGen = views traverseFlatProdInt (:[])

selectFlatProdInt :: FlatProd -> [Int]
selectFlatProdInt (FlatProd a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
  = [a1, a4]

eqTree :: WTree Int Int -> Bool
eqTree t = t == t

-- Map on a Tree
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = go
    where
        go (TLeaf x) = TLeaf (f x)
        go (Bin l r) = Bin (go l) (go r)

mapTreeInc :: Tree Int -> Tree Int
mapTreeInc = mapTree (+1)

renumberTree :: Tree Int -> Tree Int
renumberTree l = evalState (renumberInt l) 0

renumberLogic :: Logic -> Logic
renumberLogic l = evalState (renumberIntLogic l) 0

rmWeights :: WTree Int Int -> WTree Int Int
rmWeights (WithWeight t w) = rmWeights t
rmWeights (Leaf x)         = Leaf x
rmWeights (Fork l r)       = Fork (rmWeights l) (rmWeights r)

selectDumb :: WTree Int Int -> [Int]
selectDumb t = selectIntDumb t

selectSmart :: WTree Int Int -> [Int]
selectSmart t = selectIntSmart t

selectIntSmart :: WTree Int Int -> [Int]
selectIntSmart (Leaf x) = [x]
selectIntSmart (Fork l r) = selectIntSmart l ++ selectIntSmart r
selectIntSmart (WithWeight t w) = w : selectIntSmart t

selectIntDumb :: WTree Int Int -> [Int]
selectIntDumb (Leaf x) = [x]
selectIntDumb (Fork l r) = selectIntDumb l ++ selectIntDumb r
selectIntDumb (WithWeight t w) = selectIntDumb t ++ [w]

selectInt_acc :: WTree Int Int -> [Int]
selectInt_acc = loop []
  where
    loop acc (Leaf x) = x : acc
    loop acc (Fork l r) = loop (loop acc r) l
    loop acc (WithWeight t w) = loop (w : acc) t

updateLogicConst :: Logic -> Logic
updateLogicConst l = updateLogic (const 'y') l

updateHsModuleConst :: HsModule -> HsModule
updateHsModuleConst hs = updateHsModule (const 'y') hs

{-# INLINE getUnique #-}
getUnique :: State Int Int
getUnique = do
    u <- get
    modify (+1)
    return u

renumberInt :: Tree Int -> State Int (Tree Int)
renumberInt (TLeaf _) = TLeaf <$> getUnique
renumberInt (Bin l r)       = do
    l' <- renumberInt l
    r' <- renumberInt r
    return $ Bin l' r'

{-
-- In order to be fair with SYB, which is not selective, we descend into Strings.
renumberIntString :: String -> State Int String
renumberIntString [] = return []
renumberIntString (c:cs) = renumberIntString cs >>= return . (c:)
-}

renumberIntLogic :: Logic -> State Int Logic
renumberIntLogic (Var s _i) = do
    i' <- getUnique
--    s' <- renumberIntString s
    return $ Var s i'
renumberIntLogic T = return T
renumberIntLogic F = return F
renumberIntLogic (Not l) = do
    l' <- renumberIntLogic l
    return $ Not l'
renumberIntLogic (Impl l r) = do
    l' <- renumberIntLogic l
    r' <- renumberIntLogic r
    return $ Impl l' r'
renumberIntLogic (Equiv l r) = do
    l' <- renumberIntLogic l
    r' <- renumberIntLogic r
    return $ Equiv l' r'
renumberIntLogic (Conj l r) = do
    l' <- renumberIntLogic l
    r' <- renumberIntLogic r
    return $ Conj l' r'
renumberIntLogic (Disj l r) = do
    l' <- renumberIntLogic l
    r' <- renumberIntLogic r
    return $ Disj l' r'



updateLogic :: (Char -> Char) -> Logic -> Logic
updateLogic f l = go l
  where
    go (Var s i)   = Var (map f s) i
    go (Impl  p q) = Impl  (go p) (go q)
    go (Equiv p q) = Equiv (go p) (go q)
    go (Conj  p q) = Conj  (go p) (go q)
    go (Disj  p q) = Disj  (go p) (go q)
    go (Not p)     = Not (go p)
    go T           = T
    go F           = F

-------------------------------

--- the logic
-- use this definition to prevent benefits from static-arg'd map
-- updateString [] = []
-- updateString (_:ys) = 'y' : updateString ys
updateString _ [] = []
updateString f (y:ys) = f y : updateString f ys

updateChar f = f
--- end logic

updateHsName f (HsIdent s) = HsIdent (updateString f s)
updateHsName f (HsSymbol s) = HsSymbol (updateString f s)

updateHsNames f = Prelude.map (updateHsName f)

updateHsLiteral f (HsChar c) = HsChar (updateChar f c)
updateHsLiteral f (HsString s) = HsString (updateString f s)
updateHsLiteral f (HsInt i) = HsInt (updateInteger f i)
updateHsLiteral f (HsFrac r) = HsFrac (updateRational f r)
updateHsLiteral f (HsCharPrim c) = HsCharPrim (updateChar f c)
updateHsLiteral f (HsStringPrim s) = HsStringPrim (updateString f s)
updateHsLiteral f (HsIntPrim i) = HsIntPrim (updateInteger f i)
updateHsLiteral f (HsFloatPrim r) = HsFloatPrim (updateRational f r)
updateHsLiteral f (HsDoublePrim r) = HsDoublePrim (updateRational f r)

updateSrcLoc f (SrcLoc s i1 i2) = SrcLoc (updateString f s) (updateInt f i1) (updateInt f i2)
updateModule f (Module s) = Module (updateString f s)

updateBool _ b = b
updateMaybeModule _ Nothing = Nothing
updateMaybeModule f (Just m) = Just (updateModule f m)
updateInt _ i = i
updateInteger _ i = i
updateRational f (i1 :% i2) = (updateInteger f i1) :% (updateInteger f i2)

updateHsCNames f = Prelude.map (updateHsCName f)
updateHsCName f (HsVarName hsName) = HsVarName (updateHsName f hsName)
updateHsCName f (HsConName hsName) = HsConName (updateHsName f hsName)

updateHsQNames f = Prelude.map (updateHsQName f)
updateHsQName f (Qual modul hsName) = Qual (updateModule f modul) (updateHsName f hsName)
updateHsQName f (UnQual hsName) = UnQual (updateHsName f hsName)
updateHsQName f (Special hsSpecialCon) = Special (updateHsSpecialCon f hsSpecialCon)

updateHsSpecialCon _ HsUnitCon = HsUnitCon
updateHsSpecialCon _ HsListCon = HsListCon
updateHsSpecialCon _ HsFunCon = HsFunCon
updateHsSpecialCon f (HsTupleCon i) = HsTupleCon (updateInt f i)
updateHsSpecialCon _ HsCons = HsCons

-----------
-- Module

updateHsModule :: (Char -> Char) -> HsModule -> HsModule
updateHsModule f (HsModule srcLoc modul maybeHsExportSpecs hsImportDecls hsDecls) = HsModule (updateSrcLoc f srcLoc) (updateModule f modul) (updateMaybeHsExportSpecs f maybeHsExportSpecs) (updateHsImportDecls f hsImportDecls) (updateHsDecls f hsDecls)

updateMaybeHsExportSpecs _ Nothing = Nothing
updateMaybeHsExportSpecs f (Just hsExportSpecs) = Just (Prelude.map (updateHsExportSpec f) hsExportSpecs)
updateHsExportSpecs f = Prelude.map (updateHsExportSpec f)
updateHsExportSpec f (HsEVar hsQName) = HsEVar (updateHsQName f hsQName)
updateHsExportSpec f (HsEAbs hsQName) = HsEAbs (updateHsQName f hsQName)
updateHsExportSpec f (HsEThingAll hsQName) = HsEThingAll (updateHsQName f hsQName)
updateHsExportSpec f (HsEThingWith hsQName hsCNames) = HsEThingWith (updateHsQName f hsQName) (updateHsCNames f hsCNames)
updateHsExportSpec f (HsEModuleContents modul) = HsEModuleContents (updateModule f modul)


updateHsImportSpecs f = Prelude.map (updateHsImportSpec f)
updateHsImportSpec f (HsIVar hsName) = HsIVar (updateHsName f hsName)
updateHsImportSpec f (HsIAbs hsName) = HsIAbs (updateHsName f hsName)
updateHsImportSpec f (HsIThingAll hsName) = HsIThingAll (updateHsName f hsName)
updateHsImportSpec f (HsIThingWith hsName hsCNames) = HsIThingWith (updateHsName f hsName) (updateHsCNames f hsCNames)

updateHsImportDecls f = Prelude.map (updateHsImportDecl f)
updateHsImportDecl f (HsImportDecl srcLoc modul bool maybeModule maybeBoolHsImportSpecs) = HsImportDecl (updateSrcLoc f srcLoc) (updateModule f modul) (updateBool f bool) (updateMaybeModule f maybeModule) (updateMaybeBoolHsImportSpecs f maybeBoolHsImportSpecs)
updateMaybeBoolHsImportSpecs _ Nothing = Nothing
updateMaybeBoolHsImportSpecs f (Just (bool, hsImportSpecs)) = Just (updateBool f bool, updateHsImportSpecs f hsImportSpecs)

-----------------
-- Decl

updateHsDecls f = Prelude.map (updateHsDecl f)
updateHsDecl f (HsTypeDecl srcLoc hsName hsNames hsType) = HsTypeDecl (updateSrcLoc f srcLoc) (updateHsName f hsName) (updateHsNames f hsNames) (updateHsType f hsType)
updateHsDecl f (HsDataDecl srcLoc hsContext hsName hsNames hsConDecls hsQNames) = HsDataDecl (updateSrcLoc f srcLoc) (updateHsContext f hsContext) (updateHsName f hsName) (updateHsNames f hsNames) (updateHsConDecls f hsConDecls) (updateHsQNames f hsQNames)
updateHsDecl f (HsInfixDecl srcLoc hsAssoc int hsOps) = HsInfixDecl (updateSrcLoc f srcLoc) (updateHsAssoc f hsAssoc) (updateInt f int) (updateHsOps f hsOps)
updateHsDecl f (HsNewTypeDecl srcLoc hsContext hsName hsNames hsConDecl hsQNames) = HsNewTypeDecl (updateSrcLoc f srcLoc) (updateHsContext f hsContext) (updateHsName f hsName) (updateHsNames f hsNames) (updateHsConDecl f hsConDecl) (updateHsQNames f hsQNames)
updateHsDecl f (HsClassDecl srcLoc hsContext hsName hsNames hsDecls) = HsClassDecl (updateSrcLoc f srcLoc) (updateHsContext f hsContext) (updateHsName f hsName) (updateHsNames f hsNames) (updateHsDecls f hsDecls)
updateHsDecl f (HsInstDecl srcLoc hsContext hsQName hsTypes hsDecls) = HsInstDecl (updateSrcLoc f srcLoc) (updateHsContext f hsContext) (updateHsQName f hsQName) (updateHsTypes f hsTypes) (updateHsDecls f hsDecls)
updateHsDecl f (HsDefaultDecl srcLoc hsTypes) = HsDefaultDecl (updateSrcLoc f srcLoc) (updateHsTypes f hsTypes)
updateHsDecl f (HsTypeSig srcLoc hsNames hsQualType) = HsTypeSig (updateSrcLoc f srcLoc) (updateHsNames f hsNames) (updateHsQualType f hsQualType)
updateHsDecl f (HsFunBind hsMatchs) = HsFunBind (updateHsMatchs f hsMatchs)
updateHsDecl f (HsPatBind srcLoc hsPat hsRhs hsDecls) = HsPatBind (updateSrcLoc f srcLoc) (updateHsPat f hsPat) (updateHsRhs f hsRhs) (updateHsDecls f hsDecls)
updateHsDecl f (HsForeignImport srcLoc string1 hsSafety string2 hsName hsType) = HsForeignImport (updateSrcLoc f srcLoc) (updateString f string1) (updateHsSafety f hsSafety) (updateString f string2) (updateHsName f hsName) (updateHsType f hsType)
updateHsDecl f (HsForeignExport srcLoc string1 string2 hsName hsType) = HsForeignExport (updateSrcLoc f srcLoc) (updateString f string1) (updateString f string2) (updateHsName f hsName) (updateHsType f hsType)

updateHsConDecls f = Prelude.map (updateHsConDecl f)
updateHsConDecl f (HsConDecl srcLoc hsName hsBangTypes) = HsConDecl (updateSrcLoc f srcLoc) (updateHsName f hsName) (updateHsBangTypes f hsBangTypes)
updateHsConDecl f (HsRecDecl srcLoc hsName hsNamesHsBangTypes) = HsRecDecl (updateSrcLoc f srcLoc) (updateHsName f hsName) (updateHsNamesHsBangTypes f hsNamesHsBangTypes)

updateHsNamesHsBangTypes f = Prelude.map (updateHsNamesHsBangType f)
updateHsNamesHsBangType f (hsNames, hsBangType) = (,) (updateHsNames f hsNames) (updateHsBangType f hsBangType)
updateHsBangTypes f = Prelude.map (updateHsBangType f)
updateHsBangType f (HsBangedTy hsType) = HsBangedTy (updateHsType f hsType)
updateHsBangType f (HsUnBangedTy hsType) = HsUnBangedTy (updateHsType f hsType)

updateHsAssoc _ HsAssocNone = HsAssocNone
updateHsAssoc _ HsAssocLeft = HsAssocLeft
updateHsAssoc _ HsAssocRight = HsAssocRight

updateHsOps f = Prelude.map (updateHsOp f)
updateHsOp f (HsVarOp hsName) = HsVarOp (updateHsName f hsName)
updateHsOp f (HsConOp hsName) = HsConOp (updateHsName f hsName)

updateHsMatchs f = Prelude.map (updateHsMatch f)
updateHsMatch f (HsMatch srcLoc hsName hsPats hsRhs hsDecls) = HsMatch (updateSrcLoc f srcLoc) (updateHsName f hsName) (updateHsPats f hsPats) (updateHsRhs f hsRhs) (updateHsDecls f hsDecls)

updateHsRhs f (HsUnGuardedRhs hsExp) = HsUnGuardedRhs (updateHsExp f hsExp)
updateHsRhs f (HsGuardedRhss hsGuardedRhss) = HsGuardedRhss (updateHsGuardedRhss f hsGuardedRhss)

updateHsGuardedRhss f = Prelude.map (updateHsGuardedRhs f)
updateHsGuardedRhs f (HsGuardedRhs srcLoc hsExp1 hsExp2) = HsGuardedRhs (updateSrcLoc f srcLoc) (updateHsExp f hsExp1) (updateHsExp f hsExp2)
updateHsSafety _ HsSafe = HsSafe
updateHsSafety _ HsUnsafe = HsUnsafe

-----------------
-- Types

updateHsTypes f = Prelude.map (updateHsType f)
updateHsType f (HsTyFun hsType1 hsType2) = HsTyFun (updateHsType f hsType1) (updateHsType f hsType2)
updateHsType f (HsTyTuple hsTypes) = HsTyTuple (updateHsTypes f hsTypes)
updateHsType f (HsTyApp hsType1 hsType2) = HsTyApp (updateHsType f hsType1) (updateHsType f hsType2)
updateHsType f (HsTyVar hsName) = HsTyVar (updateHsName f hsName)
updateHsType f (HsTyCon hsQName) = HsTyCon (updateHsQName f hsQName)

updateHsQualType f (HsQualType hsContext hsType) = HsQualType (updateHsContext f hsContext) (updateHsType f hsType)
updateHsContext f = Prelude.map (updateHsAsst f)
updateHsAsst f (hsQName, hsTypes) = (,) (updateHsQName f hsQName) (updateHsTypes f hsTypes)

---------------
-- Patterns

updateHsPats f = Prelude.map (updateHsPat f)
updateHsPat f (HsPVar hsName) = HsPVar (updateHsName f hsName)
updateHsPat f (HsPLit hsLiteral) = HsPLit (updateHsLiteral f hsLiteral)
updateHsPat f (HsPNeg hsPat) = HsPNeg (updateHsPat f hsPat)
updateHsPat f (HsPInfixApp hsPat1 hsQName hsPat2) = HsPInfixApp (updateHsPat f hsPat1) (updateHsQName f hsQName) (updateHsPat f hsPat2)
updateHsPat f (HsPApp hsQName hsPats) = HsPApp (updateHsQName f hsQName) (updateHsPats f hsPats)
updateHsPat f (HsPTuple hsPats) = HsPTuple (updateHsPats f hsPats)
updateHsPat f (HsPList hsPats) = HsPList (updateHsPats f hsPats)
updateHsPat f (HsPParen hsPat) = HsPParen (updateHsPat f hsPat)
updateHsPat f (HsPRec hsQName hsPatFields) = HsPRec (updateHsQName f hsQName) (updateHsPatFields f hsPatFields)
updateHsPat f (HsPAsPat hsName hsPat) = HsPAsPat (updateHsName f hsName) (updateHsPat f hsPat)
updateHsPat f (HsPWildCard) = HsPWildCard
updateHsPat f (HsPIrrPat hsPat) = HsPIrrPat (updateHsPat f hsPat)

updateHsPatFields f = Prelude.map (updateHsPatField f)
updateHsPatField f (HsPFieldPat hsQName hsPat) = HsPFieldPat (updateHsQName f hsQName) (updateHsPat f hsPat)

----------------
-- Exp

updateHsExps f = Prelude.map (updateHsExp f)
updateHsExp f (HsVar hsQName) = HsVar (updateHsQName f hsQName)
updateHsExp f (HsCon hsQName) = HsCon (updateHsQName f hsQName)
updateHsExp f (HsLit hsLiteral) = HsLit (updateHsLiteral f hsLiteral)
updateHsExp f (HsInfixApp hsExp1 hsQOp hsExp2) = HsInfixApp (updateHsExp f hsExp1) (updateHsQOp f hsQOp) (updateHsExp f hsExp2)
updateHsExp f (HsApp hsExp1 hsExp2) = HsApp (updateHsExp f hsExp1) (updateHsExp f hsExp2)
updateHsExp f (HsNegApp hsExp) = HsNegApp (updateHsExp f hsExp)
updateHsExp f (HsLambda srcLoc hsPats hsExp) = HsLambda (updateSrcLoc f srcLoc) (updateHsPats f hsPats) (updateHsExp f hsExp)
updateHsExp f (HsLet hsDecls hsExp) = HsLet (updateHsDecls f hsDecls) (updateHsExp f hsExp)
updateHsExp f (HsIf hsExp1 hsExp2 hsExp3) = HsIf (updateHsExp f hsExp1) (updateHsExp f hsExp2) (updateHsExp f hsExp3)
updateHsExp f (HsCase hsExp hsAlts) = HsCase (updateHsExp f hsExp) (updateHsAlts f hsAlts)
updateHsExp f (HsDo hsStmts) = HsDo (updateHsStmts f hsStmts)
updateHsExp f (HsTuple hsExps) = HsTuple (updateHsExps f hsExps)
updateHsExp f (HsList hsExps) = HsList (updateHsExps f hsExps)
updateHsExp f (HsParen hsExp) = HsParen (updateHsExp f hsExp)
updateHsExp f (HsLeftSection hsExp hsQOp) = HsLeftSection (updateHsExp f hsExp) (updateHsQOp f hsQOp)
updateHsExp f (HsRightSection hsQOp hsExp) = HsRightSection (updateHsQOp f hsQOp) (updateHsExp f hsExp)
updateHsExp f (HsRecConstr hsQName hsFieldUpdates) = HsRecConstr (updateHsQName f hsQName) (updateHsFieldUpdates f hsFieldUpdates)
updateHsExp f (HsRecUpdate hsExp hsFieldUpdates) = HsRecUpdate (updateHsExp f hsExp) (updateHsFieldUpdates f hsFieldUpdates)
updateHsExp f (HsEnumFrom hsExp) = HsEnumFrom (updateHsExp f hsExp)
updateHsExp f (HsEnumFromTo hsExp1 hsExp2) = HsEnumFromTo (updateHsExp f hsExp1) (updateHsExp f hsExp2)
updateHsExp f (HsEnumFromThen hsExp1 hsExp2) = HsEnumFromThen (updateHsExp f hsExp1) (updateHsExp f hsExp2)
updateHsExp f (HsEnumFromThenTo hsExp1 hsExp2 hsExp3) = HsEnumFromThenTo (updateHsExp f hsExp1) (updateHsExp f hsExp2) (updateHsExp f hsExp3)
updateHsExp f (HsListComp hsExp hsStmts) = HsListComp (updateHsExp f hsExp) (updateHsStmts f hsStmts)
updateHsExp f (HsExpTypeSig srcLoc hsExp hsQualType) = HsExpTypeSig (updateSrcLoc f srcLoc) (updateHsExp f hsExp) (updateHsQualType f hsQualType)
updateHsExp f (HsAsPat hsName hsExp) = HsAsPat (updateHsName f hsName) (updateHsExp f hsExp)
updateHsExp f (HsWildCard) = HsWildCard
updateHsExp f (HsIrrPat hsExp) = HsIrrPat (updateHsExp f hsExp)

updateHsQOp f (HsQVarOp hsQName) = HsQVarOp (updateHsQName f hsQName)
updateHsQOp f (HsQConOp hsQName) = HsQConOp (updateHsQName f hsQName)

updateHsAlts f = Prelude.map (updateHsAlt f)
updateHsAlt f (HsAlt srcLoc hsPat hsGuardedAlts hsDecls) = HsAlt (updateSrcLoc f srcLoc) (updateHsPat f hsPat) (updateHsGuardedAlts f hsGuardedAlts) (updateHsDecls f hsDecls)
updateHsGuardedAlts f (HsUnGuardedAlt hsExp) = HsUnGuardedAlt (updateHsExp f hsExp)
updateHsGuardedAlts f (HsGuardedAlts hsGuardedAltList) = HsGuardedAlts (updateHsGuardedAltList f hsGuardedAltList)
updateHsGuardedAltList f = Prelude.map (updateHsGuardedAlt f)
updateHsGuardedAlt f (HsGuardedAlt srcLoc hsExp1 hsExp2) = HsGuardedAlt (updateSrcLoc f srcLoc) (updateHsExp f hsExp1) (updateHsExp f hsExp2)

updateHsStmts f = Prelude.map (updateHsStmt f)
updateHsStmt f (HsGenerator srcLoc hsPat hsExp) = HsGenerator (updateSrcLoc f srcLoc) (updateHsPat f hsPat) (updateHsExp f hsExp)
updateHsStmt f (HsQualifier hsExp) = HsQualifier (updateHsExp f hsExp)
updateHsStmt f (HsLetStmt hsDecls) = HsLetStmt (updateHsDecls f hsDecls)

updateHsFieldUpdates f = Prelude.map (updateHsFieldUpdate f)
updateHsFieldUpdate f (HsFieldUpdate hsQName hsExp) = HsFieldUpdate (updateHsQName f hsQName) (updateHsExp f hsExp)
