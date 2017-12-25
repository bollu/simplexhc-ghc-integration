module Sxhc.Plugin (plugin) where
import GhcPlugins
import CoreToStg
import CorePrep
import StgSyn
import Data.Foldable
import Data.Traversable
import Outputable

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
}

-- ifPprDebug
foofoo :: SDoc -> SDoc
foofoo = id -- Outputable.ifPprDebug


printStgExpr :: StgExpr -> SDoc
printStgExpr (StgLit lit)     = ppr lit

-- general case
printStgExpr (StgApp func args)
  = hang (ppr func) 4 (sep (map (ppr) args))

printStgExpr (StgConApp con args _)
  = hsep [ ppr con, brackets (interppSP args) ]

printStgExpr (StgOpApp op args _)
  = hsep [ printStgOp op, brackets (interppSP args)]

printStgExpr (StgLam bndrs body)
  = sep [ char '\\' <+> ppr_list (map (pprBndr LambdaBind) bndrs)
            <+> text "->",
         printStgExpr body ]
  where ppr_list = brackets . fsep . punctuate comma

-- special case: let v = <very specific thing>
--               in
--               let ...
--               in
--               ...
--
-- Very special!  Suspicious! (SLPJ)

{-
printStgExpr (StgLet srt (StgNonRec bndr (StgRhsClosure cc bi free_vars upd_flag args rhs))
                        expr@(StgLet _ _))
  = ($$)
      (hang (hcat [text "let { ", ppr bndr, ptext (sLit " = "),
                          ppr cc,
                          pp_binder_info bi,
                          text " [", ifPprDebug (interppSP free_vars), ptext (sLit "] \\"),
                          ppr upd_flag, text " [",
                          interppSP args, char ']'])
            8 (sep [hsep [ppr rhs, text "} in"]]))
      (ppr expr)
-}

-- special case: let ... in let ...

printStgExpr (StgLet bind expr@(StgLet _ _))
  = ($$)
      (sep [hang (text "let {")
                2 (hsep [printStgBinding bind, text "} in"])])
      (ppr expr)

-- general case
printStgExpr (StgLet bind expr)
  = sep [hang (text "let {") 2 (printStgBinding bind),
           hang (text "} in ") 2 (ppr expr)]

printStgExpr (StgLetNoEscape bind expr)
  = sep [hang (text "let-no-escape {")
                2 (printStgBinding bind),
           hang (text "} in ")
                2 (ppr expr)]

printStgExpr (StgTick tickish expr)
  = sdocWithDynFlags $ \dflags ->
    if gopt Opt_SuppressTicks dflags
    then printStgExpr expr
    else sep [ ppr tickish, printStgExpr expr ]


printStgExpr (StgCase expr bndr alt_type alts)
  = sep [sep [text "case",
          nest 4 (hsep [printStgExpr expr,
            (foofoo (dcolon <+> ppr alt_type))]),
          text "of", pprBndr CaseBind bndr, char '{'],
          nest 2 (vcat (map printStgAlt alts)),
          char '}']
printStgExpr _ = text "expr"

printStgAlt :: (OutputableBndr bndr, Outputable occ, Ord occ)
          => GenStgAlt bndr occ -> SDoc
printStgAlt (con, params, expr)
  = hang (hsep [ppr con, sep (map (pprBndr CasePatBind) params), text "->"])
         4 (ppr expr Outputable.<> semi)

printStgOp :: StgOp -> SDoc
printStgOp (StgPrimOp  op)   = ppr op
printStgOp (StgPrimCallOp op)= ppr op
printStgOp (StgFCallOp op _) = ppr op



-- Not yet done.
printStgRhs :: StgRhs -> SDoc
printStgRhs (StgRhsClosure ccs binderInfo occs updatable bndrs expr) =  printStgExpr expr 
printStgRhs (StgRhsCon ccs dataConstructor args) = hcat [ppr dataConstructor, brackets (interppSP args)]

printStgRhs rhs = text "rhs"

printStgBinding :: StgBinding -> SDoc
printStgBinding (StgNonRec _ rhs) = printStgRhs rhs
printStgBinding binders = text "binders"


printStgTopBinding :: StgTopBinding -> SDoc
printStgTopBinding (StgTopStringLit _ bytestring) = text "stringLit"
printStgTopBinding (StgTopLifted top) = printStgBinding top


printStgProgram :: DynFlags -> [StgTopBinding] -> CoreM ()
printStgProgram flags binds = forM_ binds (putMsgS . showSDoc flags . printStgTopBinding) 

-- bindsOnlyPass :: (CoreProgram -> CoreM CoreProgram) -> ModGuts -> CoreM ModGuts
-- coreToStg :: coreToStg :: DynFlags -> Module -> CoreProgram -> [StgTopBinding] 
pass :: ModGuts -> CoreM ModGuts
pass guts = do
  flags <- getDynFlags
  mod <- getModule
  env <- getHscEnv

  let coreProgram = mg_binds guts
  let tycons = mg_tcs guts
  let modloc = ModLocation {
    ml_hs_file  = Nothing,
    ml_hi_file  = "Example.hi",
     ml_obj_file = "Example.o"
   }
  preppedProgram <- liftIO $ corePrepPgm env mod modloc coreProgram  tycons

  let stgProgram = coreToStg flags mod preppedProgram
  printStgProgram flags stgProgram

  putMsgS "Hello!Hello!"
  return guts

coreToDoFromPass :: CoreToDo
coreToDoFromPass = CoreDoPluginPass "DumpSxhcStg" pass


install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  return $ todo ++ [coreToDoFromPass]
