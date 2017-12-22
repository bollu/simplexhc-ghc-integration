module Sxhc.Plugin (plugin) where
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
	installCoreToDos = install
}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  putMsgS "Hello!"
  return todo
