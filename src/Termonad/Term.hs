
module Termonad.Term where

import Termonad.Prelude

import Control.Lens

import Termonad.Types

-- TODO: Rewrite these types of functions
focusTerm :: Int -> TMState -> IO ()
focusTerm i tmState = do
  modifyMVar_ tmState $ \oldTMState@TMState{tmStateNotebook} -> do
    let tabs = tmNotebookTabs tmStateNotebook
        maybeNewTabs = updateFocusFL i tabs
    case maybeNewTabs of
      Nothing -> pure oldTMState
      Just (notebookTab, newTabs) -> do
        notebookSetCurrentPage (tmNotebook tmNotebookTabs) (fromIntegral i)
        let term = notebookTab .^ lensTMNotebookTabTerm . lensTerm
        setWidgetHasFocus term True
        let newTMState =
              oldTMState $ lensTMStateNotebook . lensTMNotebookTabs .~ newTabs
        pure newTMState

altNumSwitchTerm :: Int -> TMState -> IO ()
altNumSwitchTerm = focusTerm

termExit :: ScrolledWindow -> TMNotebookTab -> TMState -> Int32 -> IO ()
termExit scrolledWin tab mvarTMState _exitStatus =
  modifyMVar_ mvarTMState $ \tmState -> do
    let notebook = tmStateNotebook tmState
        note = tmNotebook notebook
        oldTabs = tmNotebookTabs notebook
    #detachTab note scrolledWin
    let newTabs = deleteFL oldTabs tab
    let newTMState =
          set (lensTMStateNotebook . lensTMNotebookTabs) newTabs tmState
    pure newTMState

createScrolledWin :: IO ScrolledWindow
createScrolledWin = do
  scrolledWin <- new ScrolledWindow []
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
        note = tmNotebook tmStateNotebook
        tabs = tmNotebookTabs tmStateNotebook
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
    termExit scrolledWin tmTerm mvarTMState
  pure tmTerm
