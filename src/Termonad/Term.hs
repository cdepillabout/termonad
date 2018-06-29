
module Termonad.Term where

import Termonad.Prelude

import Termonad.Types

-- TODO: Rewrite these types of functions
focusTerm :: Int -> TMState -> IO ()
focusTerm i tmState = do
  modifyMVar_ tmState $ \oldTMState@TMState{..} -> do
    let tabs = tmNotebookTabs tmStateNotebook
        maybeNewTabs = setFocusFL i tabs
    case maybeNewTabs of
      Nothing -> pure oldTMState
      Just newTabs -> do
        notebookSetCurrentPage (tmNotebook tmNotebookTabs) (fromIntegral i)
        let newTMState =
              oldTMState $ lensTMStateNotebook . lensTMNotebookTabs .~ newTabs
        pure newTMState

altNumSwitchTerm :: Int -> TMState -> IO ()
altNumSwitchTerm = focusTerm
  Note{..} <- readMVar tmState
  void $ #setCurrentPage notebook (fromIntegral i)

focusTerm :: Term -> IO ()
focusTerm Term{..} =
  Gdk.set term [#hasFocus := True]

termExit :: ScrolledWindow -> Term -> TMState -> Int32 -> IO ()
termExit scrolledWin terminal tmState _exitStatus =
  modifyMVar_ tmState $ \Note{..} -> do
    #detachTab notebook scrolledWin
    pure $ Note notebook (removeTerm children terminal) font

createScrolledWin :: IO ScrolledWindow
createScrolledWin = do
  scrolledWin <- new ScrolledWindow []
  #show scrolledWin
  pure scrolledWin

createTerm :: (TMState -> EventKey -> IO Bool) -> TMState -> IO Term
createTerm handleKeyPress tmState = do
  scrolledWin <- createScrolledWin
  fontDesc <- withMVar tmState (pure . font)
  vteTerm <-
    new Terminal [#fontDesc := fontDesc, #cursorBlinkMode := CursorBlinkModeOn]
  _termResVal <-
    #spawnSync
      vteTerm
      [PtyFlagsDefault]
      Nothing
      ["/usr/bin/env", "bash"]
      Nothing
      [SpawnFlagsDefault]
      Nothing
      noCancellable
  #show vteTerm
  uniq' <- newUnique
  let terminal = Term vteTerm uniq'
  #add scrolledWin (term terminal)
  modifyMVar_ tmState $ \Note{..} -> do
    pageIndex <- #appendPage notebook scrolledWin noWidget
    void $ #setCurrentPage notebook pageIndex
    pure $ Note notebook (snoc children terminal) font
  void $ Gdk.on vteTerm #windowTitleChanged $ do
    title <- get vteTerm #windowTitle
    Note{..} <- readMVar tmState
    #setTabLabelText notebook scrolledWin title
  void $ Gdk.on (term terminal) #keyPressEvent $ handleKeyPress tmState
  void $ Gdk.on scrolledWin #keyPressEvent $ handleKeyPress tmState
  void $ Gdk.on (term terminal) #childExited $ termExit scrolledWin terminal tmState
  pure terminal
