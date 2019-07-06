{-# LANGUAGE CPP #-}

module Termonad.Term where

import Termonad.Prelude

import Control.Lens ((^.), (&), (.~), set, to)
import Data.Colour.SRGB (Colour, RGB(RGB), toSRGB)
import Data.FocusList (appendFL, deleteFL, getFocusItemFL)
import GI.Gdk
  ( EventKey
  , RGBA
  , newZeroRGBA
  , setRGBABlue
  , setRGBAGreen
  , setRGBARed
  )
import GI.Gio
  ( noCancellable
  )
import GI.GLib
  ( SpawnFlags(SpawnFlagsDefault)
  )
import GI.Gtk
  ( Align(AlignFill)
  , ApplicationWindow
  , Box
  , Button
  , IconSize(IconSizeMenu)
  , Label
  , Notebook
  , Orientation(OrientationHorizontal)
  , PolicyType(PolicyTypeAlways, PolicyTypeAutomatic, PolicyTypeNever)
  , ReliefStyle(ReliefStyleNone)
  , ResponseType(ResponseTypeNo, ResponseTypeYes)
  , ScrolledWindow
  , applicationGetActiveWindow
  , boxNew
  , buttonNewFromIconName
  , buttonSetRelief
  , containerAdd
  , dialogAddButton
  , dialogGetContentArea
  , dialogNew
  , dialogRun
  , labelNew
  , labelSetEllipsize
  , labelSetLabel
  , labelSetMaxWidthChars
  , noAdjustment
  , notebookAppendPage
  , notebookDetachTab
  , notebookGetNPages
  , notebookPageNum
  , notebookSetCurrentPage
  , notebookSetShowTabs
  , notebookSetTabReorderable
  , onButtonClicked
  , onWidgetKeyPressEvent
  , scrolledWindowNew
  , scrolledWindowSetPolicy
  , setWidgetMargin
  , widgetDestroy
  , widgetGrabFocus
  , widgetSetCanFocus
  , widgetSetHalign
  , widgetSetHexpand
  , widgetShow
  , windowSetFocus
  , windowSetTransientFor
  )
import GI.Pango (EllipsizeMode(EllipsizeModeMiddle), FontDescription)
import GI.Vte
  ( PtyFlags(PtyFlagsDefault)
  , Terminal
  , onTerminalChildExited
  , onTerminalWindowTitleChanged
  , terminalGetWindowTitle
  , terminalNew
  , terminalSetCursorBlinkMode
  , terminalSetFont
  , terminalSetScrollbackLines
  , terminalSpawnSync
  , terminalSetWordCharExceptions
  )
import System.FilePath ((</>))
import System.Directory (getSymbolicLinkTarget)
import System.Environment (lookupEnv)

import Termonad.Lenses
  ( lensConfirmExit
  , lensOptions
  , lensShowScrollbar
  , lensShowTabBar
  , lensTMNotebookTabLabel
  , lensTMNotebookTabTerm
  , lensTMNotebookTabTermContainer
  , lensTMNotebookTabs
  , lensTMStateApp
  , lensTMStateConfig
  , lensTMStateNotebook
  , lensTerm
  )
import Termonad.Types
  ( ConfigHooks(createTermHook)
  , ConfigOptions(scrollbackLen, wordCharExceptions, cursorBlinkMode)
  , ShowScrollbar(..)
  , ShowTabBar(..)
  , TMConfig(hooks, options)
  , TMNotebook
  , TMNotebookTab
  , TMState
  , TMState'(TMState, tmStateAppWin, tmStateConfig, tmStateFontDesc, tmStateNotebook)
  , TMTerm
  , assertInvariantTMState
  , createTMNotebookTab
  , newTMTerm
  , pid
  , tmNotebook
  , tmNotebookTabTerm
  , tmNotebookTabTermContainer
  , tmNotebookTabs
  )

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
        tmState ^. lensTMStateNotebook . lensTMNotebookTabs . to getFocusItemFL
  case maybeTab of
    Nothing -> pure ()
    Just tab -> termClose tab mvarTMState

termClose :: TMNotebookTab -> TMState -> IO ()
termClose tab mvarTMState = do
  tmState <- readMVar mvarTMState
  let confirm = tmState ^. lensTMStateConfig . lensOptions . lensConfirmExit
      close = if confirm then termExitWithConfirmation else termExit
  close tab mvarTMState

termExitWithConfirmation :: TMNotebookTab -> TMState -> IO ()
termExitWithConfirmation tab mvarTMState = do
  tmState <- readMVar mvarTMState
  let app = tmState ^. lensTMStateApp
  win <- applicationGetActiveWindow app
  dialog <- dialogNew
  box <- dialogGetContentArea dialog
  label <- labelNew (Just "Close tab?")
  containerAdd box label
  widgetShow label
  setWidgetMargin label 10
  void $
    dialogAddButton
      dialog
      "No, do NOT close tab"
      (fromIntegral (fromEnum ResponseTypeNo))
  void $
    dialogAddButton
      dialog
      "Yes, close tab"
      (fromIntegral (fromEnum ResponseTypeYes))
  windowSetTransientFor dialog win
  res <- dialogRun dialog
  widgetDestroy dialog
  case toEnum (fromIntegral res) of
    ResponseTypeYes -> termExit tab mvarTMState
    _ -> pure ()

termExit :: TMNotebookTab -> TMState -> IO ()
termExit tab mvarTMState = do
  detachTabAction <-
    modifyMVar mvarTMState $ \tmState -> do
      let notebook = tmStateNotebook tmState
          detachTabAction =
            notebookDetachTab
              (tmNotebook notebook)
              (tmNotebookTabTermContainer tab)
      let newTabs = deleteFL tab (tmNotebookTabs notebook)
      let newTMState =
            set (lensTMStateNotebook . lensTMNotebookTabs) newTabs tmState
      pure (newTMState, detachTabAction)
  detachTabAction
  relabelTabs mvarTMState

relabelTabs :: TMState -> IO ()
relabelTabs mvarTMState = do
  TMState{tmStateNotebook} <- readMVar mvarTMState
  let notebook = tmNotebook tmStateNotebook
      tabFocusList = tmNotebookTabs tmStateNotebook
  foldMap (go notebook) tabFocusList
  where
    go :: Notebook -> TMNotebookTab -> IO ()
    go notebook tmNotebookTab = do
      let label = tmNotebookTab ^. lensTMNotebookTabLabel
          scrolledWin = tmNotebookTab ^. lensTMNotebookTabTermContainer
          term' = tmNotebookTab ^. lensTMNotebookTabTerm . lensTerm
      relabelTab notebook label scrolledWin term'

relabelTab :: Notebook -> Label -> ScrolledWindow -> Terminal -> IO ()
relabelTab notebook label scrolledWin term' = do
  pageNum <- notebookPageNum notebook scrolledWin
  maybeTitle <- terminalGetWindowTitle term'
  let title = fromMaybe "shell" maybeTitle
  labelSetLabel label $ tshow (pageNum + 1) <> ". " <> title

showScrollbarToPolicy :: ShowScrollbar -> PolicyType
showScrollbarToPolicy ShowScrollbarNever = PolicyTypeNever
showScrollbarToPolicy ShowScrollbarIfNeeded = PolicyTypeAutomatic
showScrollbarToPolicy ShowScrollbarAlways = PolicyTypeAlways

createScrolledWin :: TMState -> IO ScrolledWindow
createScrolledWin mvarTMState = do
  tmState <- readMVar mvarTMState
  let showScrollbarVal = tmState ^. lensTMStateConfig . lensOptions . lensShowScrollbar
      vScrollbarPolicy = showScrollbarToPolicy showScrollbarVal
  scrolledWin <- scrolledWindowNew noAdjustment noAdjustment
  widgetShow scrolledWin
  scrolledWindowSetPolicy scrolledWin PolicyTypeAutomatic vScrollbarPolicy
  pure scrolledWin

createNotebookTabLabel :: IO (Box, Label, Button)
createNotebookTabLabel = do
  box <- boxNew OrientationHorizontal 5
  label <- labelNew (Just "")
  labelSetEllipsize label EllipsizeModeMiddle
  labelSetMaxWidthChars label 10
  widgetSetHexpand label True
  widgetSetHalign label AlignFill
  button <-
    buttonNewFromIconName
      (Just "window-close")
      (fromIntegral (fromEnum IconSizeMenu))
  buttonSetRelief button ReliefStyleNone
  containerAdd box label
  containerAdd box button
  widgetSetCanFocus button False
  widgetSetCanFocus label False
  widgetSetCanFocus box False
  widgetShow box
  widgetShow label
  widgetShow button
  pure (box, label, button)

setShowTabs :: TMConfig -> Notebook -> IO ()
setShowTabs tmConfig note = do
  npages <- notebookGetNPages note
  let shouldShowTabs =
        case tmConfig ^. lensOptions . lensShowTabBar of
          ShowTabBarIfNeeded -> npages > 1
          ShowTabBarAlways   -> True
          ShowTabBarNever    -> False
  notebookSetShowTabs note shouldShowTabs

toRGBA :: Colour Double -> IO RGBA
toRGBA colour = do
  let RGB red green blue = toSRGB colour
  rgba <- newZeroRGBA
  setRGBARed rgba red
  setRGBAGreen rgba green
  setRGBABlue rgba blue
  pure rgba

-- | TODO: This should probably be implemented in an external package,
-- since it is a generally useful utility.
--
-- It should also be implemented for windows and osx.
cwdOfPid :: Int -> IO (Maybe Text)
cwdOfPid pd = do
#ifdef mingw32_HOST_OS
  pure Nothing
#else
#ifdef darwin_HOST_OS
  pure Nothing
#else
  let pidPath = "/proc" </> show pd </> "cwd"
  eitherLinkTarget <- try $ getSymbolicLinkTarget pidPath
  case eitherLinkTarget of
    Left (_ :: IOException) -> pure Nothing
    Right linkTarget -> pure $ Just $ pack linkTarget
#endif
#endif

-- | Get the directory from the current focused tab of a notebook
getDirFromFocusedTab :: TMNotebook -> IO (Maybe Text)
getDirFromFocusedTab currNote = do
  let maybeCurrFocusedTabPid = pid . tmNotebookTabTerm <$> getFocusItemFL (tmNotebookTabs currNote)
  maybe (pure Nothing) cwdOfPid maybeCurrFocusedTabPid

-- | Create the VTE terminal, set the fonts and options
createAndInitVteTerm :: FontDescription -> ConfigOptions -> IO (Terminal)
createAndInitVteTerm tmStateFontDesc curOpts = do
  vteTerm <- terminalNew
  terminalSetFont vteTerm (Just tmStateFontDesc)
  terminalSetWordCharExceptions vteTerm $ wordCharExceptions curOpts
  terminalSetScrollbackLines vteTerm (fromIntegral (scrollbackLen curOpts))
  terminalSetCursorBlinkMode vteTerm (cursorBlinkMode curOpts)
  widgetShow vteTerm
  pure vteTerm

-- | Starts a shell in a terminal and return a new TMTerm
launchShell :: Terminal -> Maybe (Text) -> IO (TMTerm)
launchShell vteTerm maybeCurrDir = do
  -- Should probably use GI.Vte.Functions.getUserShell, but contrary to its
  -- documentation it raises an exception rather wrap in Maybe.
  mShell <- lookupEnv "SHELL"
  let argv = fromMaybe ["/usr/bin/env", "bash"] (pure <$> mShell)
  -- Launch the shell
  terminalProcPid <-
    terminalSpawnSync
      vteTerm
      [PtyFlagsDefault]
      maybeCurrDir
      argv
      Nothing
      ([SpawnFlagsDefault] :: [SpawnFlags])
      Nothing
      noCancellable
  -- Init the state associated to the terminal
  newTMTerm vteTerm (fromIntegral terminalProcPid)

-- | Add a page to the notebook and update TMState' accordingly
addPage :: TMNotebookTab -> Box -> TMState' -> IO (TMState', IO())
addPage notebookTab tabLabelBox tmState = do
  let notebook = tmStateNotebook tmState
      note = tmNotebook notebook
      tabs = tmNotebookTabs notebook
      scrolledWin = tmNotebookTabTermContainer notebookTab
  pageIndex <- notebookAppendPage note scrolledWin (Just tabLabelBox)
  notebookSetTabReorderable note scrolledWin True
  setShowTabs (tmState ^. lensTMStateConfig) note
  -- Append the new
  let newTabs = appendFL tabs notebookTab
      newTMState =
        tmState & lensTMStateNotebook . lensTMNotebookTabs .~ newTabs
      mvarReturnAction = notebookSetCurrentPage note pageIndex
  pure (newTMState, mvarReturnAction)

-- | Set the keyboard focus on a vte terminal
setFocusOn :: ApplicationWindow -> Terminal -> IO()
setFocusOn tmStateAppWin vteTerm = do
  widgetGrabFocus vteTerm
  windowSetFocus tmStateAppWin (Just vteTerm)

createTerm :: (TMState -> EventKey -> IO Bool) -> TMState -> IO TMTerm
createTerm handleKeyPress mvarTMState = do
  -- Check preconditions
  assertInvariantTMState mvarTMState

  -- Read needed data in TMVar
  TMState{tmStateAppWin, tmStateFontDesc, tmStateConfig, tmStateNotebook=currNote} <-
    readMVar mvarTMState

  -- Create a new terminal and launch a shell in it
  vteTerm <- createAndInitVteTerm tmStateFontDesc (options tmStateConfig)
  maybeCurrDir <- getDirFromFocusedTab currNote
  tmTerm <- launchShell vteTerm maybeCurrDir

  -- Create the container add the VTE term in it
  scrolledWin <- createScrolledWin mvarTMState
  containerAdd scrolledWin vteTerm

  -- Create the widget for the tab
  (tabLabelBox, tabLabel, tabCloseButton) <- createNotebookTabLabel
  -- Create notebook state
  let notebookTab = createTMNotebookTab tabLabel scrolledWin tmTerm
  relabelTab (tmNotebook currNote) tabLabel scrolledWin vteTerm
  
  -- Add a new page and update mvar
  setCurrentPageAction <-
    modifyMVar mvarTMState $ addPage notebookTab tabLabelBox
  setCurrentPageAction

  -- Connect callbacks
  void $ onButtonClicked tabCloseButton $ termClose notebookTab mvarTMState
  void $ onTerminalWindowTitleChanged vteTerm $ do
    TMState{tmStateNotebook} <- readMVar mvarTMState
    let notebook = tmNotebook tmStateNotebook
    relabelTab notebook tabLabel scrolledWin vteTerm
  void $ onWidgetKeyPressEvent vteTerm $ handleKeyPress mvarTMState
  void $ onWidgetKeyPressEvent scrolledWin $ handleKeyPress mvarTMState
  void $ onTerminalChildExited vteTerm $ \_ -> termExit notebookTab mvarTMState

  -- Put the keyboard focus on the term
  setFocusOn tmStateAppWin vteTerm
  -- Make sure the state is still right
  assertInvariantTMState mvarTMState
  createTermHook (hooks tmStateConfig) mvarTMState vteTerm
  pure tmTerm


