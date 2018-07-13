module Termonad.Term where

import Termonad.Prelude

import Control.Lens
import GI.Gdk
  ( EventKey
  )
import GI.Gio
  ( noCancellable
  )
import GI.GLib
  ( SpawnFlags(SpawnFlagsDefault)
  )
import GI.Gtk
  ( Application
  , ApplicationWindow(ApplicationWindow)
  , Box(Box)
  , CssProvider(CssProvider)
  , Dialog(Dialog)
  , Notebook(Notebook)
  , ScrolledWindow(ScrolledWindow)
  , pattern STYLE_PROVIDER_PRIORITY_APPLICATION
  , applicationNew
  , applicationSetAccelsForAction
  , builderNewFromString
  , builderSetApplication
  , containerAdd
  , noAdjustment
  , notebookAppendPage
  , notebookDetachTab
  , notebookSetCurrentPage
  , notebookSetTabLabelText
  , noWidget
  , onWidgetKeyPressEvent
  , scrolledWindowNew
  , setWidgetHasFocus
  , styleContextAddProviderForScreen
  , widgetGrabFocus
  , widgetShow
  )
import GI.Vte
  ( CursorBlinkMode(CursorBlinkModeOn)
  , PtyFlags(PtyFlagsDefault)
  , onTerminalChildExited
  , onTerminalWindowTitleChanged
  , terminalGetWindowTitle
  , terminalNew
  , terminalSetCursorBlinkMode
  , terminalSetFont
  , terminalSpawnSync
  )

import Termonad.FocusList
import Termonad.Types
import Termonad.Types (lensTMNotebookTabs)

import Text.Pretty.Simple

focusTerm :: Int -> TMState -> IO ()
focusTerm i mvarTMState = do
  note <- tmNotebook . tmStateNotebook <$> readMVar mvarTMState
  notebookSetCurrentPage note (fromIntegral i)

altNumSwitchTerm :: Int -> TMState -> IO ()
altNumSwitchTerm = focusTerm

termExitFocused :: TMState -> IO ()
termExitFocused mvarTMState = do
  tmState <- readMVar mvarTMState
  let maybeTab =
        tmState ^. lensTMStateNotebook . lensTMNotebookTabs . to getFLFocusItem
  case maybeTab of
    Nothing -> pure ()
    Just tab -> termExit tab mvarTMState

termExit :: TMNotebookTab -> TMState -> IO ()
termExit tab mvarTMState = do
  detachTabAction <-
    modifyMVar mvarTMState $ \tmState -> do
      putStrLn "In termExit', tmState:"
      pPrint tmState
      let notebook = tmStateNotebook tmState
          detachTabAction =
            notebookDetachTab
              (tmNotebook notebook)
              (tmNotebookTabTermContainer tab)
      let newTabs = deleteFL tab (tmNotebookTabs notebook)
      let newTMState =
            set (lensTMStateNotebook . lensTMNotebookTabs) newTabs tmState
      putStrLn "In termExit', newTMState:"
      pPrint newTMState
      pure (newTMState, detachTabAction)
  detachTabAction

createScrolledWin :: IO ScrolledWindow
createScrolledWin = do
  scrolledWin <- scrolledWindowNew noAdjustment noAdjustment
  widgetShow scrolledWin
  pure scrolledWin

createTerm :: (TMState -> EventKey -> IO Bool) -> TMState -> IO TMTerm
createTerm handleKeyPress mvarTMState = do
  putStrLn "createTerm, started..."
  withMVar mvarTMState (\tmState -> putStrLn $ "createTerm, started tmState: " <> tshow tmState)
  scrolledWin <- createScrolledWin
  TMState{tmStateFontDesc} <- readMVar mvarTMState
  vteTerm <- terminalNew
  terminalSetFont vteTerm (Just tmStateFontDesc)
  terminalSetCursorBlinkMode vteTerm CursorBlinkModeOn
  widgetShow vteTerm
  widgetGrabFocus $ vteTerm
  _termResVal <-
    terminalSpawnSync
      vteTerm
      [PtyFlagsDefault]
      Nothing
      ["/usr/bin/env", "bash"]
      Nothing
      ([SpawnFlagsDefault] :: [SpawnFlags])
      Nothing
      noCancellable
  tmTerm <- newTMTerm vteTerm
  let notebookTab = createTMNotebookTab scrolledWin tmTerm
  containerAdd scrolledWin vteTerm
  putStrLn "createTerm, created some stuff, about to go into mvar..."
  setCurrPageAction <-
    modifyMVar mvarTMState $ \tmState -> do
      putStrLn "createTerm, in mvar thing, started..."
      let notebook = tmStateNotebook tmState
          note = tmNotebook notebook
          tabs = tmNotebookTabs notebook
      putStrLn "createTerm, in mvar thing, about to notebookAppendPage..."
      pageIndex <- notebookAppendPage note scrolledWin noWidget
      let newTabs = appendFL tabs notebookTab
          newTMState =
            tmState & lensTMStateNotebook . lensTMNotebookTabs .~ newTabs
          setCurrPageAction = do
            notebookSetCurrentPage note pageIndex
      pure (newTMState, setCurrPageAction)
  putStrLn "createTerm, after mvar thing..."
  putStrLn "createTerm, in mvar thing, about to notebookSetCurrentPage..."
  setCurrPageAction
  void $ onTerminalWindowTitleChanged vteTerm $ do
    title <- terminalGetWindowTitle vteTerm
    TMState{tmStateNotebook} <- readMVar mvarTMState
    let notebook = tmNotebook tmStateNotebook
    notebookSetTabLabelText notebook scrolledWin title
  void $ onWidgetKeyPressEvent vteTerm $ handleKeyPress mvarTMState
  void $ onWidgetKeyPressEvent scrolledWin $ handleKeyPress mvarTMState
  void $ onTerminalChildExited vteTerm $ \_ -> termExit notebookTab mvarTMState
  withMVar mvarTMState (\tmState -> putStrLn $ "createTerm, ending tmState: " <> tshow tmState)
  pure tmTerm
