
module Termonad.Term where


altNumSwitchTerm :: Int -> TerState -> IO ()
altNumSwitchTerm i terState = do
  Note{..} <- readMVar terState
  void $ #setCurrentPage notebook (fromIntegral i)

focusTerm :: Term -> IO ()
focusTerm Term{..} =
  Gdk.set term [#hasFocus := True]

termExit :: ScrolledWindow -> Term -> TerState -> Int32 -> IO ()
termExit scrolledWin terminal terState _exitStatus =
  modifyMVar_ terState $ \Note{..} -> do
    #detachTab notebook scrolledWin
    pure $ Note notebook (removeTerm children terminal) font

createScrolledWin :: IO ScrolledWindow
createScrolledWin = do
  scrolledWin <- new ScrolledWindow []
  #show scrolledWin
  pure scrolledWin

createTerm :: TerState -> IO Term
createTerm terState = do
  scrolledWin <- createScrolledWin
  fontDesc <- withMVar terState (pure . font)
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
  modifyMVar_ terState $ \Note{..} -> do
    pageIndex <- #appendPage notebook scrolledWin noWidget
    void $ #setCurrentPage notebook pageIndex
    pure $ Note notebook (snoc children terminal) font
  void $ Gdk.on vteTerm #windowTitleChanged $ do
    title <- get vteTerm #windowTitle
    Note{..} <- readMVar terState
    #setTabLabelText notebook scrolledWin title
  void $ Gdk.on (term terminal) #keyPressEvent $ handleKeyPress terState
  void $ Gdk.on scrolledWin #keyPressEvent $ handleKeyPress terState
  void $ Gdk.on (term terminal) #childExited $ termExit scrolledWin terminal terState
  pure terminal
