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

focusTerm :: Int -> TMState -> IO ()
focusTerm i tmState = do
  modifyMVar_ tmState $ \oldTMState@TMState{tmStateNotebook} -> do
    let tabs = tmNotebookTabs tmStateNotebook
        maybeNewTabs = updateFocusFL i tabs
    case maybeNewTabs of
      Nothing -> pure oldTMState
      Just (notebookTab, newTabs) -> do
        notebookSetCurrentPage (tmNotebook tmStateNotebook) (fromIntegral i)
        let term = notebookTab ^. lensTMNotebookTabTerm . lensTerm
        setWidgetHasFocus term True
        let newTMState =
              oldTMState & lensTMStateNotebook . lensTMNotebookTabs .~ newTabs
        pure newTMState

altNumSwitchTerm :: Int -> TMState -> IO ()
altNumSwitchTerm = focusTerm

termExit :: ScrolledWindow -> TMNotebookTab -> TMState -> Int32 -> IO ()
termExit scrolledWin tab mvarTMState _exitStatus =
  modifyMVar_ mvarTMState $ \tmState -> do
    let notebook = tmStateNotebook tmState
        note = tmNotebook notebook
        oldTabs = tmNotebookTabs notebook
    notebookDetachTab note scrolledWin
    let newTabs = deleteFL tab oldTabs
    let newTMState =
          set (lensTMStateNotebook . lensTMNotebookTabs) newTabs tmState
    pure newTMState

createScrolledWin :: IO ScrolledWindow
createScrolledWin = do
  scrolledWin <- scrolledWindowNew noAdjustment noAdjustment
  widgetShow scrolledWin
  pure scrolledWin

createTerm :: (TMState -> EventKey -> IO Bool) -> TMState -> IO TMTerm
createTerm handleKeyPress mvarTMState = do
  scrolledWin <- createScrolledWin
  TMState{tmStateFontDesc} <- readMVar mvarTMState
  vteTerm <- terminalNew
  terminalSetFont vteTerm (Just tmStateFontDesc)
  terminalSetCursorBlinkMode vteTerm CursorBlinkModeOn
  _termResVal <-
    terminalSpawnSync
      vteTerm
      [PtyFlagsDefault]
      Nothing
      ["/usr/bin/env", "bash"]
      Nothing
      [SpawnFlagsDefault]
      Nothing
      noCancellable
  widgetShow vteTerm
  tmTerm <- newTMTerm vteTerm
  let notebookTab = createTMNotebookTab scrolledWin tmTerm
  containerAdd scrolledWin vteTerm
  modifyMVar_ mvarTMState $ \tmState -> do
    let notebook = tmStateNotebook tmState
        note = tmNotebook notebook
        tabs = tmNotebookTabs notebook
    pageIndex <- notebookAppendPage note scrolledWin noWidget
    void $ notebookSetCurrentPage note pageIndex
    let newTabs = appendFL tabs notebookTab
    pure $ tmState & lensTMStateNotebook . lensTMNotebookTabs .~ newTabs
  void $ onTerminalWindowTitleChanged vteTerm $ do
    title <- terminalGetWindowTitle vteTerm
    TMState{tmStateNotebook} <- readMVar mvarTMState
    let notebook = tmNotebook tmStateNotebook
    notebookSetTabLabelText notebook scrolledWin title
  void $ onWidgetKeyPressEvent vteTerm $ handleKeyPress mvarTMState
  void $ onWidgetKeyPressEvent scrolledWin $ handleKeyPress mvarTMState
  void $ onTerminalChildExited vteTerm $
    termExit scrolledWin notebookTab mvarTMState
  pure tmTerm
