module Main where

import Criterion
import Criterion.Main
import qualified Hand.Examples as H
import qualified SYB.Examples as SYB
import qualified GenericLens.Examples as GL
import qualified Uniplate.Examples as U
import qualified Geniplate.Examples as G
import qualified OneLiner.Examples as O
import qualified Lens.Examples as L
import Auxiliary.Tree (bigTree, smallerTree)
import Auxiliary.Logic (bigLogic)
import Auxiliary.WTree
import Auxiliary.FlatProd
import Language.Haskell.Parser
import Language.Haskell.Syntax

import Control.DeepSeq

bigWTree = mkFullWTree 53

readHsModule :: IO HsModule
readHsModule = do
  res <- readFile "src/Auxiliary/Tree.hs"
  case parseModule res of
    ParseOk mod -> return mod
    ParseFailed ss s -> error $  "Parse failed: " ++ show ss ++ s



hsModEnv = do
  hsModule <- readHsModule
  return hsModule

hsModBench name f = env hsModEnv (\h -> bench name $ nf f h)
treeBench name f = env bigTreeEnv (\h -> bench name $ nf f h)
streeBench name f = env sTreeEnv (\h -> bench name $ nf f h)
wtreeBench name f = env bigWTreeEnv (\h -> bench name $ nf f h)
logicBench name f = env logicEnv (\h -> bench name $ nf f h)
flatProdBench name f = env flatProdEnv (\h -> bench name $ nf f h)

bigTreeEnv = return bigTree
sTreeEnv = return smallerTree
bigWTreeEnv = return bigWTree
logicEnv = return bigLogic
flatProdEnv = return (FlatProd 5 'a' True 4 'b' False "a" "b" "c" "d")


main = defaultMain [
   -- notice the lazy pattern match here!
   bgroup "main" [
   bgroup "flatprod/update" [
      flatProdBench "hand" (H.updateFlatProdInt (+1))
--      , flatProdBench "hand-gen" H.updateFlatProdGen
      , flatProdBench "SYB" (SYB.updateFlatProdInt (+1))
      , flatProdBench "gl" (GL.updateFlatProdInt (+1))
      , flatProdBench "uniplate" (U.updateFlatProdInt (+1))
      , flatProdBench "geniplate" (G.updateFlatProdInt (+1))
      , flatProdBench "one-liner" (O.updateFlatProdInt (+1))
      , flatProdBench "lens" (L.updateFlatProdInt (+1))
    ]
  , bgroup "flatprod/select" [
      flatProdBench "hand" H.selectFlatProdInt
--      , flatProdBench "hand-gen" H.selectFlatProdGen
      , flatProdBench "SYB" SYB.selectFlatProdInt
      , flatProdBench "gl" GL.selectFlatProdInt
      , flatProdBench "uniplate" U.selectFlatProdInt
      , flatProdBench "geniplate" G.selectFlatProdInt
      , flatProdBench "one-liner" O.selectFlatProdInt
      , flatProdBench "lens" L.selectFlatProdInt
    ]
  , bgroup "eq" [
      wtreeBench "hand"  H.eqTree
      , wtreeBench "syb"  SYB.eqTree
      , wtreeBench "one-liner"  O.eqTree
   ]
  , bgroup "map/small" [
      streeBench "hand"  H.mapTreeInc
      , streeBench "syb"  SYB.mapTreeInc
      , streeBench "gl"  GL.mapTreeInc
      , streeBench "uniplate"  U.mapTreeInc
      , streeBench "geniplate"  G.mapTreeInc
      , streeBench "one-liner"  O.mapTreeInc -- one-liner has a big space leak
      , streeBench "lens"  L.mapTreeInc
   ]
 ,  bgroup "map" [
      treeBench "hand"  H.mapTreeInc
      , treeBench "syb"  SYB.mapTreeInc
      , treeBench "gl"  GL.mapTreeInc
      , treeBench "uniplate"  U.mapTreeInc
      , treeBench "geniplate"  G.mapTreeInc
      , treeBench "lens"  L.mapTreeInc
--      , treeBench "one-liner"  O.mapTreeInc
   ]
 ,  bgroup "rmWeights" [
      wtreeBench "hand"  H.rmWeights
      , wtreeBench "syb"  SYB.rmWeights
      , wtreeBench "gl"  GL.rmWeights
      , wtreeBench "geniplate"  G.rmWeights
      , wtreeBench "one-liner"  O.rmWeights
      , wtreeBench "lens"  L.rmWeights

   ]
 ,  bgroup "select" [
      wtreeBench "hand"  H.selectDumb
      , wtreeBench "syb"  SYB.selectDumb
      , wtreeBench "gl"  GL.selectDumb
      , wtreeBench "uniplate"  U.selectDumb
      , wtreeBench "geniplate"  G.selectDumb
      , wtreeBench "one-liner"  O.selectDumb
      , wtreeBench "lens"  L.selectDumb
   ]
 ,  bgroup "selectSmart" [
      wtreeBench "hand"  H.selectSmart
      , wtreeBench "syb"  SYB.selectSmart
      , wtreeBench "gl"  GL.selectSmart
      , wtreeBench "one-liner"  O.selectSmart
      , wtreeBench "lens"  L.selectSmart
   ]
 ,  bgroup "update/logic" [
      logicBench "hand"  H.updateLogicConst
      , logicBench "syb"  SYB.updateLogicConst
      , logicBench "gl"  GL.updateLogicConst
      , logicBench "uniplate"  U.updateLogicConst
      , logicBench "geniplate"  G.updateLogicConst
      , logicBench "one-liner"  O.updateLogicConst
      , logicBench "lens"  L.updateLogicConst
   ]
 ,  bgroup "update/hsmodchar" [
      hsModBench "hand"  H.updateHsModuleConst
      , hsModBench "syb"  SYB.updateHsModuleConst
      , hsModBench "gl"  GL.updateHsModuleConst
      , hsModBench "uniplate"  U.updateHsModuleConst
      , hsModBench "geniplate"  G.updateHsModuleConst
      , hsModBench "one-liner"  O.updateHsModuleConst
      , hsModBench "lens"  L.updateHsModuleConst
   ]
 ,  bgroup "update/hsmodassoc" [
        hsModBench "gl"  GL.setHsModAssoc
      , hsModBench "geniplate"  G.setHsModAssoc
      , hsModBench "one-liner"  O.setHsModAssoc
   ]

 ,  bgroup "renumberTree" [
      treeBench "hand"  H.renumberTree
      , treeBench "syb"  SYB.renumberTree
      , treeBench "gl"  GL.renumberTree
      , treeBench "uniplate"  U.renumberTree
      , treeBench "geniplate"  G.renumberTree
      , treeBench "lens"  L.renumberTree
      , treeBench "one-liner"  O.renumberTree
   ]

 ,  bgroup "renumberLogic" [
      logicBench "hand"  H.renumberLogic
      , logicBench "syb"  SYB.renumberLogic
      , logicBench "gl"  GL.renumberLogic
      , logicBench "uniplate"  U.renumberLogic
      , logicBench "geniplate"  G.renumberLogic
      , logicBench "one-liner"  O.renumberLogic
      , logicBench "lens"  L.renumberLogic

   ]
 ]  ]
