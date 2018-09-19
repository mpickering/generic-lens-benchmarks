{-# LANGUAGE CPP #-}
module Plugin (plugin) where
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  dflags <- getDynFlags
  return $ todo  ++ [CoreDoSpecialising, simpl_pass dflags ]
  where
    pp = CoreDoPluginPass "add-inline" pass
#if __GLASGOW_HASKELL__ < 804
    simpl_pass _ = CoreDoSimplify 3 (SimplMode ["last"] (Phase 0) True True True True)
#else
    simpl_pass dflags = CoreDoSimplify 3 (SimplMode ["last"] (Phase 0) dflags True True True True)
#endif

pass :: ModGuts -> CoreM ModGuts
pass mg = return $ mg { mg_binds =  progPass (mg_binds mg) }

progPass :: CoreProgram -> CoreProgram
progPass cp = map addInline cp

addInline :: Bind CoreBndr -> Bind CoreBndr
addInline (NonRec b rhs) = NonRec (mod_binder b) rhs
  where
    mod_binder var =
      let s = occNameString (getOccName var)
      in if s == "$cto" || s == "$fGenericLogic"
          then pprTrace "Adding to:" (ppr s) (modifyInlinePragma var (\_ -> alwaysInlinePragma))
          else pprTrace "Ignoring:" (ppr s) var
addInline (Rec bs) = Rec bs


