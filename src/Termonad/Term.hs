{-# LANGUAGE CPP #-}

module Termonad.Term where

import Termonad.Prelude

import Control.Lens ((^.), ix, set)
import Data.Coerce (coerce)
import Data.Colour.SRGB (Colour, RGB(RGB), toSRGB)
import Data.FocusList (appendFL, deleteFL, getFocusItemFL)
import Data.GI.Base (toManagedPtr)
import Data.Text (pack)
import qualified Data.Text as Text
import GI.Gdk
  ( Event (Event)
  , EventButton
  , EventKey
  , RGBA
  , getEventButtonButton
  , newZeroRGBA
  , setRGBABlue
  , setRGBAGreen
  , setRGBARed
  , pattern BUTTON_SECONDARY
  , pattern CURRENT_TIME
  )
import GI.Gio
  ( Cancellable
  , actionMapAddAction
  , menuAppend
  , menuNew
  , onSimpleActionActivate
  , simpleActionNew
  )
import GI.GLib
  ( SpawnFlags(SpawnFlagsDefault)
  )
import GI.Gtk
  ( Adjustment
  , Align(AlignFill)
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
  , Window
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
  , menuAttachToWidget
  , menuNewFromModel
  , menuPopupAtPointer
  , notebookAppendPage
  , notebookDetachTab
  , notebookGetNPages
  , notebookNextPage
  , notebookPageNum
  , notebookPrevPage
  , notebookSetCurrentPage
  , notebookSetShowTabs
  , notebookSetTabReorderable
  , onButtonClicked
  , onWidgetButtonPressEvent
  , onWidgetKeyPressEvent
  , scrolledWindowNew
  , scrolledWindowSetPolicy
  , setWidgetMargin
  , showUriOnWindow
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
  , regexNewForMatch
  , terminalGetAllowHyperlink
  , terminalGetWindowTitle
  , terminalMatchAddRegex
  , terminalMatchCheckEvent
  , terminalNew
  , terminalSetBoldIsBright
  , terminalSetCursorBlinkMode
  , terminalSetFont
  , terminalSetScrollbackLines
  , terminalSetWordCharExceptions
  , terminalSpawnSync
  , terminalSetAllowBold
  )
import System.Directory (getSymbolicLinkTarget)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Termonad.Gtk (terminalSetEnableSixelIfExists)
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
  , lensTerm, lensTMStateWindows, lensTMWindowNotebook
  )
import Termonad.Pcre (pcre2Multiline)
import Termonad.Types
  ( ConfigHooks(createTermHook)
  , ConfigOptions(scrollbackLen, wordCharExceptions, cursorBlinkMode, boldIsBright, enableSixel, allowBold)
  , ShowScrollbar(..)
  , ShowTabBar(..)
  , TMConfig(hooks, options)
  , TMNotebook
  , TMNotebookTab
  , TMState
  , TMState'(TMState, tmStateConfig, tmStateFontDesc)
  , TMTerm
  , TMWindowId
  , assertInvariantTMState
  , createTMNotebookTab
  , getNotebookFromTMState
  , getTMNotebookFromTMState
  , getTMNotebookFromTMState'
  , getTMWindowFromTMState
  , getTMWindowFromWins
  , newTMTerm
  , pid
  , tmNotebook
  , tmNotebookTabTerm
  , tmNotebookTabTermContainer
  , tmNotebookTabs
  , tmStateWindows
  , tmWindowAppWin
  , tmWindowNotebook
  )

focusTerm :: Int -> TMState -> TMWindowId -> IO ()
focusTerm i mvarTMState tmWinId = do
  note <- getNotebookFromTMState mvarTMState tmWinId
  notebookSetCurrentPage note (fromIntegral i)

altNumSwitchTerm :: Int -> TMState -> TMWindowId -> IO ()
altNumSwitchTerm = focusTerm

-- | Change focus to the next tab.
termNextPage :: TMState -> TMWindowId -> IO ()
termNextPage mvarTMState tmWinId = do
  note <- getNotebookFromTMState mvarTMState tmWinId
  notebookNextPage note

-- | Change focus to the previous tab.
termPrevPage :: TMState -> TMWindowId -> IO ()
termPrevPage mvarTMState tmWinId = do
  note <- getNotebookFromTMState mvarTMState tmWinId
  notebookPrevPage note

termExitFocused :: TMState -> TMWindowId -> IO ()
termExitFocused mvarTMState tmWinId = do
  note <- getTMNotebookFromTMState mvarTMState tmWinId
  let maybeTab = getFocusItemFL $ tmNotebookTabs note
  case maybeTab of
    Nothing -> pure ()
    Just tab -> termClose tab mvarTMState tmWinId

termClose :: TMNotebookTab -> TMState -> TMWindowId -> IO ()
termClose tab mvarTMState tmWindowId = do
  tmState <- readMVar mvarTMState
  let confirm = tmState ^. lensTMStateConfig . lensOptions . lensConfirmExit
      close = if confirm then termExitWithConfirmation else termExit
  close tab mvarTMState tmWindowId

termExitWithConfirmation :: TMNotebookTab -> TMState -> TMWindowId -> IO ()
termExitWithConfirmation tab mvarTMState tmWinId = do
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
    ResponseTypeYes -> termExit tab mvarTMState tmWinId
    _ -> pure ()

termExit :: TMNotebookTab -> TMState -> TMWindowId -> IO ()
termExit tab mvarTMState tmWinId = do
  detachTabAction <-
    modifyMVar mvarTMState $ \tmState -> do
      tmNote <- getTMNotebookFromTMState' tmState tmWinId
      let detachTabAction :: IO ()
          detachTabAction =
            notebookDetachTab
              (tmNotebook tmNote)
              (tmNotebookTabTermContainer tab)
          newTabs = deleteFL tab (tmNotebookTabs tmNote)
          newTMState =
            set
              (lensTMStateWindows . ix tmWinId . lensTMWindowNotebook . lensTMNotebookTabs)
              newTabs
              tmState
      pure (newTMState, detachTabAction)
  detachTabAction
  tmWin <- getTMWindowFromTMState mvarTMState tmWinId
  relabelTabs (tmWindowNotebook tmWin)

relabelTabs :: TMNotebook -> IO ()
relabelTabs tmNote = do
  let notebook = tmNotebook tmNote
      tabFocusList = tmNotebookTabs tmNote
  foldMap (go notebook) tabFocusList
  where
    go :: Notebook -> TMNotebookTab -> IO ()
    go notebook tmNotebookTab = do
      let label = tmNotebookTab ^. lensTMNotebookTabLabel
          scrolledWin = tmNotebookTab ^. lensTMNotebookTabTermContainer
          term' = tmNotebookTab ^. lensTMNotebookTabTerm . lensTerm
      relabelTab notebook label scrolledWin term'

-- | Compute the text for a 'Label' for a GTK Notebook tab.
--
-- >>> computeTabLabel 0 (Just "me@machine:~")
-- "1. me@machine:~"
--
-- >>> computeTabLabel 5 (Just "bash process")
-- "6. bash process"
--
-- >>> computeTabLabel 2 Nothing
-- "3. shell"
computeTabLabel
  :: Int
  -- ^ Tab number.  0 is used for the first tab, 1 for the second, etc.
  -> Maybe Text
  -- ^ A possible title for a tab.  If this is 'Nothing', then the string
  -- @shell@ will be used.
  -> Text
computeTabLabel pageNum maybeTitle =
  let title = fromMaybe "shell" maybeTitle
  in tshow (pageNum + 1) <> ". " <> title

-- | Update the given 'Label' for a GTK Notebook tab.
--
-- The new text for the label is determined by the 'computeTabLabel' function.
relabelTab :: Notebook -> Label -> ScrolledWindow -> Terminal -> IO ()
relabelTab notebook label scrolledWin term' = do
  tabNum <- notebookPageNum notebook scrolledWin
  maybeTitle <- terminalGetWindowTitle term'
  let labelText = computeTabLabel (fromIntegral tabNum) maybeTitle
  labelSetLabel label labelText

showScrollbarToPolicy :: ShowScrollbar -> PolicyType
showScrollbarToPolicy ShowScrollbarNever = PolicyTypeNever
showScrollbarToPolicy ShowScrollbarIfNeeded = PolicyTypeAutomatic
showScrollbarToPolicy ShowScrollbarAlways = PolicyTypeAlways

createScrolledWin :: TMState -> IO ScrolledWindow
createScrolledWin mvarTMState = do
  tmState <- readMVar mvarTMState
  let showScrollbarVal =
        tmState ^. lensTMStateConfig . lensOptions . lensShowScrollbar
      vScrollbarPolicy = showScrollbarToPolicy showScrollbarVal
  scrolledWin <-
    scrolledWindowNew
      (Nothing :: Maybe Adjustment)
      (Nothing :: Maybe Adjustment)
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

-- | Get the current working directory from the shell in the focused tab of a
-- notebook.
--
-- Returns 'Nothing' if there is no focused tab of the notebook, or the
-- current working directory could not be detected for the shell.
getCWDFromFocusedTab :: TMNotebook -> IO (Maybe Text)
getCWDFromFocusedTab currNote = do
  let maybeFocusedTab = getFocusItemFL (tmNotebookTabs currNote)
  case maybeFocusedTab of
    Nothing -> pure Nothing
    Just focusedNotebookTab -> do
      let shellPid = pid (tmNotebookTabTerm focusedNotebookTab)
      cwdOfPid shellPid

-- | Create the VTE 'Terminal', set the fonts and options
createAndInitVteTerm :: FontDescription -> ConfigOptions -> IO Terminal
createAndInitVteTerm tmStateFontDesc curOpts = do
  vteTerm <- terminalNew
  terminalSetFont vteTerm (Just tmStateFontDesc)
  terminalSetWordCharExceptions vteTerm $ wordCharExceptions curOpts
  terminalSetScrollbackLines vteTerm (fromIntegral (scrollbackLen curOpts))
  terminalSetCursorBlinkMode vteTerm (cursorBlinkMode curOpts)
  terminalSetBoldIsBright vteTerm (boldIsBright curOpts)
  terminalSetEnableSixelIfExists vteTerm (enableSixel curOpts)
  terminalSetAllowBold vteTerm (allowBold curOpts)
  widgetShow vteTerm
  pure vteTerm

-- | Starts a shell in a terminal and return a new TMTerm
launchShell
  :: Terminal
  -- ^ GTK 'Terminal' to spawn the shell in.
  -> Maybe Text
  -- ^ An optional path to the current working directory to start the
  -- shell in.  If 'Nothing', use the current working directory of the
  -- termonad process.
  -> IO Int
launchShell vteTerm maybeCurrDir = do
  -- Should probably use GI.Vte.Functions.getUserShell, but contrary to its
  -- documentation it raises an exception rather wrap in Maybe.
  mShell <- lookupEnv "SHELL"
  let argv = maybe ["/usr/bin/env", "bash"] pure mShell
  -- Launch the shell
  shellPid <-
    terminalSpawnSync
      vteTerm
      [PtyFlagsDefault]
      maybeCurrDir
      argv
      Nothing
      ([SpawnFlagsDefault] :: [SpawnFlags])
      Nothing
      (Nothing :: Maybe Cancellable)
  pure (fromIntegral shellPid)

-- | Add a page to the notebook and switch to it.
addPage
  :: TMState
  -> TMWindowId
  -> TMNotebookTab
  -> Box
  -- ^ The GTK Object holding the label we want to show for the tab of the
  -- newly created page of the notebook.
  -> IO ()
addPage mvarTMState tmWinId notebookTab tabLabelBox = do
  -- Append a new notebook page and update the TMState to reflect this.
  (note, pageIndex) <- modifyMVar mvarTMState appendNotebookPage

  -- Switch the current Notebook page to the the newly added page.
  notebookSetCurrentPage note pageIndex
  where
    appendNotebookPage :: TMState' -> IO (TMState', (Notebook, Int32))
    appendNotebookPage tmState = do
      notebook <- getTMNotebookFromTMState' tmState tmWinId
      let note = tmNotebook notebook
          tabs = tmNotebookTabs notebook
          scrolledWin = tmNotebookTabTermContainer notebookTab
      pageIndex <- notebookAppendPage note scrolledWin (Just tabLabelBox)
      notebookSetTabReorderable note scrolledWin True
      setShowTabs (tmState ^. lensTMStateConfig) note
      let newTabs = appendFL tabs notebookTab
          newTMState =
            set
              (lensTMStateWindows . ix tmWinId . lensTMWindowNotebook . lensTMNotebookTabs)
              newTabs
              tmState
      pure (newTMState, (note, pageIndex))

-- | Set the keyboard focus on a vte terminal
setFocusOn :: ApplicationWindow -> Terminal -> IO()
setFocusOn tmStateAppWin vteTerm = do
  widgetGrabFocus vteTerm
  windowSetFocus tmStateAppWin (Just vteTerm)

-- | Create a new 'TMTerm', setting it up and adding it to the GTKNotebook.
createTerm
  :: (TMState -> TMWindowId -> EventKey -> IO Bool)
  -- ^ Funtion for handling key presses on the terminal.
  -> TMState
  -> TMWindowId
  -> IO TMTerm
createTerm handleKeyPress mvarTMState tmWinId = do
  -- Check preconditions
  assertInvariantTMState mvarTMState

  -- Read needed data in TMVar
  -- TMState{tmStateAppWin, tmStateFontDesc, tmStateConfig, tmStateNotebook=currNote} <-
  --   readMVar mvarTMState
  TMState{tmStateFontDesc, tmStateConfig, tmStateWindows} <-
    readMVar mvarTMState
  tmWin <- getTMWindowFromWins tmStateWindows tmWinId
  let appWin = tmWindowAppWin tmWin
      currNote = tmWindowNotebook tmWin

  -- Create a new terminal and launch a shell in it
  vteTerm <- createAndInitVteTerm tmStateFontDesc (options tmStateConfig)
  maybeCurrDir <- getCWDFromFocusedTab currNote
  termShellPid <- launchShell vteTerm maybeCurrDir
  tmTerm <- newTMTerm vteTerm termShellPid

  -- Create the container add the VTE term in it
  scrolledWin <- createScrolledWin mvarTMState
  containerAdd scrolledWin vteTerm

  -- Create the GTK widget for the Notebook tab
  (tabLabelBox, tabLabel, tabCloseButton) <- createNotebookTabLabel

  -- Create notebook state
  let notebookTab = createTMNotebookTab tabLabel scrolledWin tmTerm

  -- Add the new notebooktab to the notebook.
  addPage mvarTMState tmWinId notebookTab tabLabelBox

  -- Setup the initial label for the notebook tab.  This needs to happen
  -- after we add the new page to the notebook, so that the page can get labelled
  -- appropriately.
  relabelTab (tmNotebook currNote) tabLabel scrolledWin vteTerm

  -- Connect callbacks
  void $ onButtonClicked tabCloseButton $ termClose notebookTab mvarTMState tmWinId
  void $ onTerminalWindowTitleChanged vteTerm $ do
    relabelTab (tmNotebook currNote) tabLabel scrolledWin vteTerm
  void $ onWidgetKeyPressEvent vteTerm $ handleKeyPress mvarTMState tmWinId
  void $ onWidgetKeyPressEvent scrolledWin $ handleKeyPress mvarTMState tmWinId
  void $ onWidgetButtonPressEvent vteTerm $ handleMousePress appWin vteTerm
  void $ onTerminalChildExited vteTerm $ \_ -> termExit notebookTab mvarTMState tmWinId

  -- Underline URLs so that the user can see they are right-clickable.
  --
  -- This regex is from https://www.regextester.com/94502
  --
  -- TODO: Roxterm and gnome-terminal have a much more in-depth set of regexes
  -- for URLs and things similar to URLs.  At some point it might make sense to
  -- switch to something like this:
  -- https://github.com/realh/roxterm/blob/30f1faf8be4ccac8ba12b59feb5b8f758bc65a7b/src/roxterm-regex.c
  -- and
  -- https://github.com/realh/roxterm/blob/30f1faf8be4ccac8ba12b59feb5b8f758bc65a7b/src/terminal-regex.h
  let regexPat =
        "(?:http(s)?:\\/\\/)[\\w.-]+(?:\\.[\\w\\.-]+)+[\\w\\-\\._~:/?#[\\]@!\\$&'\\(\\)\\*\\+,;=.]+"
  -- We must set the pcre2Multiline option, otherwise VTE prints a warning.
  let pcreFlags = fromIntegral pcre2Multiline
  regex <- regexNewForMatch regexPat (fromIntegral $ Text.length regexPat) pcreFlags
  void $ terminalMatchAddRegex vteTerm regex 0

  -- Put the keyboard focus on the term
  setFocusOn appWin vteTerm

  -- Make sure the state is still right
  assertInvariantTMState mvarTMState

  -- Run user-defined hooks for modifying the newly-created VTE Terminal.
  createTermHook (hooks tmStateConfig) mvarTMState vteTerm
  pure tmTerm

-- | Popup the context menu on right click
handleMousePress :: ApplicationWindow -> Terminal -> EventButton -> IO Bool
handleMousePress win vteTerm eventButton = do
  x <- terminalGetAllowHyperlink vteTerm
  print x
  button <- getEventButtonButton eventButton
  let rightClick = button == fromIntegral BUTTON_SECONDARY
  when rightClick $ do
    menuModel <- menuNew

    -- if the user right-clicked on a URL, add an option to open the URL
    -- in a browser
    (maybeUrl, _regexId) <- terminalMatchCheckEvent vteTerm (eventButtonToEvent eventButton)
    case maybeUrl of
      Nothing -> pure ()
      Just url -> do
        openUrlAction <- simpleActionNew "openurl" Nothing
        void $ onSimpleActionActivate openUrlAction $ \_ ->
          showUriOnWindow (Nothing :: Maybe Window) url (fromIntegral CURRENT_TIME)
        -- This will add the openurl action to the Application Window's action
        -- map everytime the user right-clicks on a URL.  It is okay to add
        -- actions multiple times.
        actionMapAddAction win openUrlAction
        menuAppend menuModel (Just "Open URL in browser") (Just "win.openurl")


    menuAppend menuModel (Just "Copy") (Just "app.copy")
    menuAppend menuModel (Just "Paste") (Just "app.paste")
    menuAppend menuModel (Just "Preferences") (Just "app.preferences")
    menu <- menuNewFromModel menuModel
    menuAttachToWidget menu vteTerm Nothing
    menuPopupAtPointer menu Nothing
  pure rightClick

-- The terminalMatchCheckEvent function takes an Event, while we only
-- have an EventButton.  It is apparently okay to just cast an EventButton
-- to an Event, since they are just pointers under the hood, and they
-- are laid out the same in memory.  See
-- https://github.com/haskell-gi/haskell-gi/issues/109
eventButtonToEvent :: EventButton -> Event
eventButtonToEvent = Event . coerce . toManagedPtr
