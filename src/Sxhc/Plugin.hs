module Sxhc.Plugin (plugin) where
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
	installCoreToDos = install
}

pass :: ModGuts -> CoreM ModGuts
pass x = do
  flags <- getDynFlags
  mod <- getModule
  -- coreToStg  flags mod
  putMsgS "Hello!Hello!"
  return x

coreToDoFromPass :: CoreToDo
coreToDoFromPass = CoreDoPluginPass "DumpSxhcStg" pass


install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  return $ todo ++ [coreToDoFromPass]
