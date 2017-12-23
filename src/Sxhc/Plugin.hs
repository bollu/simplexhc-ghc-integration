module Sxhc.Plugin (plugin) where
import GhcPlugins
import CoreToStg
import CorePrep
import StgSyn
import Data.Foldable
import Data.Traversable

plugin :: Plugin
plugin = defaultPlugin {
	installCoreToDos = install
}

printStgProgram :: DynFlags -> [StgTopBinding] -> CoreM ()
printStgProgram flags binds = forM_ binds (putMsgS . showSDoc flags . ppr) 

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
