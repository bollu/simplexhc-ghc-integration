module Sxhc.Plugin (plugin) where
import GhcPlugins
import CoreToStg
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

  let coreProgram = mg_binds guts
  let stgProgram = coreToStg flags mod coreProgram
  printStgProgram flags stgProgram

  -- coreToStg flags (mg_module guts) prep 
  -- coreToStg  flags mod
  putMsgS "Hello!Hello!"
  -- return x
  -- bindsOnlyPass (\coreprogram -> coreToStg flags mod coreprogram) guts
  return guts

coreToDoFromPass :: CoreToDo
coreToDoFromPass = CoreDoPluginPass "DumpSxhcStg" pass


install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  return $ todo ++ [coreToDoFromPass]
