
module Termonad.Window where

import Termonad.Prelude

import Control.Lens ((^.), (^..), set, view, ix)
import Data.FocusList (focusList, moveFromToFL, updateFocusFL)
import Data.Sequence (findIndexR)
import qualified Data.Text as Text
import GI.Gdk (castTo, managedForeignPtr)
import GI.Gio
  ( actionMapAddAction
  , applicationQuit
  , onSimpleActionActivate
  , simpleActionNew
  )
import GI.Gtk
  ( Application
  , ApplicationWindow(ApplicationWindow)
  , Box(Box)
  , Notebook
  , PositionType(PositionTypeRight)
  , ResponseType(ResponseTypeNo, ResponseTypeYes)
  , Widget
  , aboutDialogNew
  , applicationAddWindow
  , applicationGetActiveWindow
  , applicationSetAccelsForAction
  , applicationWindowSetShowMenubar
  , boxPackStart
  , builderNewFromString
  , builderSetApplication
  , containerAdd
  , dialogAddButton
  , dialogGetContentArea
  , dialogNew
  , dialogResponse
  , dialogRun
  , entryGetText
  , entryNew
  , gridAttachNextTo
  , gridNew
  , labelNew
  , notebookGetNPages
  , notebookNew
  , notebookSetShowBorder
  , onEntryActivate
  , onNotebookPageRemoved
  , onNotebookPageReordered
  , onNotebookSwitchPage
  , onWidgetDeleteEvent
  , setWidgetMargin
  , widgetDestroy
  , widgetGrabFocus
  , widgetSetCanFocus
  , widgetShow
  , widgetShowAll
  , windowPresent
  , windowSetTitle
  , windowSetTransientFor
  )
import GI.Pango (FontDescription)
import GI.Vte
  ( catchRegexError
  , regexNewForSearch
  , terminalCopyClipboard
  , terminalPasteClipboard
  , terminalSearchFindNext
  , terminalSearchFindPrevious
  , terminalSearchSetRegex
  , terminalSearchSetWrapAround
  , terminalSetFont
  )
import Termonad.Keys (handleKeyPress)
import Termonad.IdMap (deleteIdMap, insertIdMap, keysIdMap, lookupIdMap)
import Termonad.Lenses
  ( lensConfirmExit
  , lensOptions
  , lensShowMenu
  , lensTMNotebookTabs
  , lensTMNotebookTabTerm
  , lensTMStateConfig
  , lensTMStateFontDesc
  , lensTMStateWindows
  , lensTMWindowNotebook
  , lensTerm
  )
import Termonad.Term
  ( createTerm
  , relabelTabs
  , setShowTabs
  , termExitFocused
  , termNextPage
  , termPrevPage
  )
import Termonad.Types
  ( FontSize
  , TMNotebookTab
  , TMState
  , TMState'(TMState, tmStateApp, tmStateConfig, tmStateWindows)
  , TMWindow
  , TMWindowId
  , assertInvariantTMState
  , createEmptyTMNotebook
  , createTMWindow
  , fontSizeFromFontDescription
  , getFocusedTermFromState
  , getTMNotebookFromTMState
  , getTMNotebookFromTMState'
  , getTMWindowFromTMState
  , setFontDescSize
  , tmNotebookTabTermContainer
  , tmNotebookTabs
  , tmWindowAppWin
  )
import Termonad.XML (interfaceText)
import Termonad.Gtk (objFromBuildUnsafe)

-- | Adjust the font size of every terminal in every window.
--
-- Font size is currently a single 'FontDescription' shared by the whole
-- application, so this necessarily affects all windows / terminals.
modifyFontSizeForAllTerms :: (FontSize -> FontSize) -> TMState -> IO ()
modifyFontSizeForAllTerms modFontSizeFunc mvarTMState = do
  tmState <- readMVar mvarTMState
  let fontDesc = tmState ^. lensTMStateFontDesc
  adjustFontDescSize modFontSizeFunc fontDesc
  let terms =
        tmState ^..
          lensTMStateWindows .
          traverse .
          lensTMWindowNotebook .
          lensTMNotebookTabs .
          traverse .
          lensTMNotebookTabTerm .
          lensTerm
  foldMap (\vteTerm -> terminalSetFont vteTerm (Just fontDesc)) terms
  where
    adjustFontDescSize :: (FontSize -> FontSize) -> FontDescription -> IO ()
    adjustFontDescSize f fontDesc = do
      currFontSz <- fontSizeFromFontDescription fontDesc
      let newFontSz = f currFontSz
      setFontDescSize fontDesc newFontSz

-- | This is the callback for when a page in a 'Notebook' has been reordered
-- (normally caused by a drag-and-drop event).
notebookPageReorderedCallback
  :: TMState
  -> TMWindowId
  -> Widget
  -- ^ The child widget that is in the Notebook page.
  -> Word32
  -- ^ The new index of the Notebook page.
  -> IO ()
notebookPageReorderedCallback mvarTMState tmWinId childWidg pageNum = do
  maybeBox <- castTo Box childWidg
  case maybeBox of
    Nothing ->
      fail $
        "In notebookPageReorderedCallback, " <>
        "child widget is not a Box.\n" <>
        "Don't know how to continue.\n"
    Just box -> do
      tmNote <- getTMNotebookFromTMState mvarTMState tmWinId
      let fl = view lensTMNotebookTabs tmNote
      let maybeOldPosition =
            findIndexR (compareBoxAndTab box) (focusList fl)
      case maybeOldPosition of
        Nothing ->
          fail $
            "In notebookPageReorderedCallback, " <>
            "the Box is not already in the FocusList.\n" <>
            "Don't know how to continue.\n"
        Just oldPos -> do
          updateFLTabPos mvarTMState tmWinId oldPos (fromIntegral pageNum)
          tmNote' <- getTMNotebookFromTMState mvarTMState tmWinId
          relabelTabs tmNote'
  where
    compareBoxAndTab :: Box -> TMNotebookTab -> Bool
    compareBoxAndTab box flTab =
      let Box managedPtrFLTab = tmNotebookTabTermContainer flTab
          foreignPtrFLTab = managedForeignPtr managedPtrFLTab
          Box managedPtrBox = box
          foreignPtrBox = managedForeignPtr managedPtrBox
      in foreignPtrFLTab == foreignPtrBox

-- | Move a 'TMNotebookTab' from one position to another.
--
-- If the current position index is out of bounds, or the new index is out of
-- bounds, then nothing will be done.
--
-- Note that this function doesn't change anything about the 'tmNotebook'.
-- This function is meant to be used as a call-back for when a 'Notebook's
-- tab-order has been changed.
updateFLTabPos
  :: TMState
  -> TMWindowId
  -> Int
  -- ^ Current position index.
  -> Int
  -- ^ New position index.
  -> IO ()
updateFLTabPos mvarTMState tmWinId oldPos newPos =
  modifyMVar_ mvarTMState $ \tmState -> do
    note <- getTMNotebookFromTMState' tmState tmWinId
    let tabs = tmNotebookTabs note
        maybeNewTabs = moveFromToFL oldPos newPos tabs
    case maybeNewTabs of
      Nothing -> do
        putStrLn $
          "in updateFLTabPos, Strange error: couldn't move tabs.\n" <>
          "old pos: " <> show oldPos <> "\n" <>
          "new pos: " <> show newPos <> "\n" <>
          "tabs: " <> show tabs <> "\n" <>
          "maybeNewTabs: " <> show maybeNewTabs <> "\n" <>
          "tmState: " <> show tmState
        pure tmState
      Just newTabs ->
        pure $
          set
            (lensTMStateWindows . ix tmWinId . lensTMWindowNotebook . lensTMNotebookTabs)
            newTabs
            tmState

showAboutDialog :: Application -> IO ()
showAboutDialog app = do
  maybeWin <- applicationGetActiveWindow app
  aboutDialog <- aboutDialogNew
  windowSetTransientFor aboutDialog maybeWin
  void $ dialogRun aboutDialog
  widgetDestroy aboutDialog

showFindDialog :: ApplicationWindow -> IO (Maybe Text)
showFindDialog win = do
  dialog <- dialogNew
  box <- dialogGetContentArea dialog
  grid <- gridNew

  searchForLabel <- labelNew (Just "Search for regex:")
  containerAdd grid searchForLabel
  widgetShow searchForLabel
  setWidgetMargin searchForLabel 10

  searchEntry <- entryNew
  gridAttachNextTo grid searchEntry (Just searchForLabel) PositionTypeRight 1 1
  widgetShow searchEntry
  setWidgetMargin searchEntry 10
  -- setWidgetMarginBottom searchEntry 20
  void $
    onEntryActivate searchEntry $
      dialogResponse dialog (fromIntegral (fromEnum ResponseTypeYes))

  void $
    dialogAddButton
      dialog
      "Close"
      (fromIntegral (fromEnum ResponseTypeNo))
  void $
    dialogAddButton
      dialog
      "Find"
      (fromIntegral (fromEnum ResponseTypeYes))

  containerAdd box grid
  widgetShow grid
  windowSetTransientFor dialog (Just win)
  res <- dialogRun dialog

  searchString <- entryGetText searchEntry
  let maybeSearchString =
        case toEnum (fromIntegral res) of
          ResponseTypeYes -> Just searchString
          _ -> Nothing

  widgetDestroy dialog

  pure maybeSearchString

doFind :: TMState -> TMWindowId -> IO ()
doFind mvarTMState tmWinId = do
  tmWin <- getTMWindowFromTMState mvarTMState tmWinId
  let win = tmWindowAppWin tmWin
  maybeSearchString <- showFindDialog win
  -- putStrLn $ "trying to find: " <> tshow maybeSearchString
  maybeTerminal <- getFocusedTermFromState mvarTMState tmWinId
  case (maybeSearchString, maybeTerminal) of
    (Just searchString, Just terminal) -> do
      -- TODO: Figure out how to import the correct pcre flags.
      --
      -- If you don't pass the pcre2Multiline flag, VTE gives
      -- the following warning:
      --
      -- (termonad-linux-x86_64:18792): Vte-WARNING **:
      -- 21:56:31.193: (vtegtk.cc:2269):void
      -- vte_terminal_search_set_regex(VteTerminal*,
      -- VteRegex*, guint32): runtime check failed:
      -- (regex == nullptr ||
      -- _vte_regex_get_compile_flags(regex) & PCRE2_MULTILINE)
      --
      -- However, if you do add the pcre2Multiline flag,
      -- the terminalSearchSetRegex appears to just completely
      -- not work.
      let pcreFlags = 0
      let newRegex =
            regexNewForSearch
              searchString
              (fromIntegral $ Text.length searchString)
              pcreFlags
      eitherRegex <-
        catchRegexError
          (fmap Right newRegex)
          (\_ errMsg -> pure (Left errMsg))
      case eitherRegex of
        Left errMsg -> do
          let msg = "error when creating regex: " <> errMsg
          hPutStrLn stderr msg
        Right regex -> do
          terminalSearchSetRegex terminal (Just regex) pcreFlags
          terminalSearchSetWrapAround terminal True
          _matchFound <- terminalSearchFindPrevious terminal
          -- TODO: Setup an actual logging framework to show these
          -- kinds of log messages.  Also make a similar change in
          -- findAbove and findBelow.
          -- putStrLn $ "was match found: " <> tshow matchFound
          pure ()
    _ -> pure ()

findAbove :: TMState -> TMWindowId -> IO ()
findAbove mvarTMState tmWinId = do
  maybeTerminal <- getFocusedTermFromState mvarTMState tmWinId
  case maybeTerminal of
    Nothing -> pure ()
    Just terminal -> do
      _matchFound <- terminalSearchFindPrevious terminal
      -- putStrLn $ "was match found: " <> tshow matchFound
      pure ()

findBelow :: TMState -> TMWindowId -> IO ()
findBelow mvarTMState tmWinId = do
  maybeTerminal <- getFocusedTermFromState mvarTMState tmWinId
  case maybeTerminal of
    Nothing -> pure ()
    Just terminal -> do
      _matchFound <- terminalSearchFindNext terminal
      -- putStrLn $ "was match found: " <> tshow matchFound
      pure ()

setupWindowCallbacks :: TMState -> Application -> ApplicationWindow -> Notebook -> TMWindowId -> IO ()
setupWindowCallbacks mvarTMState app win note tmWinId = do

  -- When a page is removed from this window's notebook: if tabs remain, just
  -- update the tab-bar visibility; if that was the last tab, close this window
  -- (which quits the whole application if it was the last window).  The config
  -- is read from the state at fire time so that runtime preference changes are
  -- honored.
  void $ onNotebookPageRemoved note $ \_ _ -> do
    pages <- notebookGetNPages note
    if pages == 0
      then do
        removed <- removeTMWindowFromState mvarTMState tmWinId
        case removed of
          Nothing -> pure ()
          Just tmWin -> widgetDestroy (tmWindowAppWin tmWin)
      else do
        tmState <- readMVar mvarTMState
        setShowTabs (tmStateConfig tmState) note

  -- The window's own close button (via the window manager).  Ask for
  -- confirmation if terminals are still running in this window; if the user
  -- confirms, drop the window from the state and let GTK destroy it.
  void $ onWidgetDeleteEvent win $ \_ -> do
    shouldClose <- askShouldCloseWindow mvarTMState win
    if shouldClose
      then do
        void $ removeTMWindowFromState mvarTMState tmWinId
        pure False -- allow GTK to proceed with destroying the window
      else pure True -- block the close

  void $ onNotebookSwitchPage note $ \_ pageNum -> do
    modifyMVar_ mvarTMState $ \tmState -> do
      tmNote <- getTMNotebookFromTMState' tmState tmWinId
      let tabs = tmNotebookTabs tmNote
          maybeNewTabs = updateFocusFL (fromIntegral pageNum) tabs
      case maybeNewTabs of
        Nothing -> pure tmState
        Just (tab, newTabs) -> do
          widgetGrabFocus $ tab ^. lensTMNotebookTabTerm . lensTerm
          pure $
            set
              (lensTMStateWindows . ix tmWinId . lensTMWindowNotebook . lensTMNotebookTabs)
              newTabs
              tmState

  void $ onNotebookPageReordered note $ \childWidg pageNum ->
    notebookPageReorderedCallback mvarTMState tmWinId childWidg pageNum

  newTabAction <- simpleActionNew "newtab" Nothing
  void $ onSimpleActionActivate newTabAction $ \_ ->
    void $ createTerm handleKeyPress mvarTMState tmWinId
  actionMapAddAction win newTabAction
  applicationSetAccelsForAction app "win.newtab" ["<Shift><Ctrl>T"]

  nextPageAction <- simpleActionNew "nextpage" Nothing
  void $ onSimpleActionActivate nextPageAction $ \_ ->
    termNextPage mvarTMState tmWinId
  actionMapAddAction win nextPageAction
  applicationSetAccelsForAction app "win.nextpage" ["<Ctrl>Page_Down"]

  prevPageAction <- simpleActionNew "prevpage" Nothing
  void $ onSimpleActionActivate prevPageAction $ \_ ->
    termPrevPage mvarTMState tmWinId
  actionMapAddAction win prevPageAction
  applicationSetAccelsForAction app "win.prevpage" ["<Ctrl>Page_Up"]

  closeTabAction <- simpleActionNew "closetab" Nothing
  void $ onSimpleActionActivate closeTabAction $ \_ ->
    termExitFocused mvarTMState tmWinId
  actionMapAddAction win closeTabAction
  applicationSetAccelsForAction app "win.closetab" ["<Shift><Ctrl>W"]

  copyAction <- simpleActionNew "copy" Nothing
  void $ onSimpleActionActivate copyAction $ \_ -> do
    maybeTerm <- getFocusedTermFromState mvarTMState tmWinId
    maybe (pure ()) terminalCopyClipboard maybeTerm
  actionMapAddAction win copyAction
  applicationSetAccelsForAction app "win.copy" ["<Shift><Ctrl>C"]

  pasteAction <- simpleActionNew "paste" Nothing
  void $ onSimpleActionActivate pasteAction $ \_ -> do
    maybeTerm <- getFocusedTermFromState mvarTMState tmWinId
    maybe (pure ()) terminalPasteClipboard maybeTerm
  actionMapAddAction win pasteAction
  applicationSetAccelsForAction app "win.paste" ["<Shift><Ctrl>V"]

  findAction <- simpleActionNew "find" Nothing
  void $ onSimpleActionActivate findAction $ \_ -> doFind mvarTMState tmWinId
  actionMapAddAction win findAction
  applicationSetAccelsForAction app "win.find" ["<Shift><Ctrl>F"]

  findAboveAction <- simpleActionNew "findabove" Nothing
  void $ onSimpleActionActivate findAboveAction $ \_ -> findAbove mvarTMState tmWinId
  actionMapAddAction win findAboveAction
  applicationSetAccelsForAction app "win.findabove" ["<Shift><Ctrl>P"]

  findBelowAction <- simpleActionNew "findbelow" Nothing
  void $ onSimpleActionActivate findBelowAction $ \_ -> findBelow mvarTMState tmWinId
  actionMapAddAction win findBelowAction
  applicationSetAccelsForAction app "win.findbelow" ["<Shift><Ctrl>I"]

-- | Remove a window from the 'TMState', quitting the whole application if it
-- was the last window.
--
-- Returns the removed 'TMWindow' (so the caller can 'widgetDestroy' its
-- 'ApplicationWindow' when appropriate), or 'Nothing' if the window had
-- already been removed.  The last-tab-closed and window-manager-close paths
-- can both fire for the same window, and returning 'Nothing' makes the second
-- one a no-op rather than a double free / double quit.
removeTMWindowFromState :: TMState -> TMWindowId -> IO (Maybe TMWindow)
removeTMWindowFromState mvarTMState tmWinId = do
  removed <-
    modifyMVar mvarTMState $ \tmState ->
      case lookupIdMap tmWinId (tmStateWindows tmState) of
        Nothing -> pure (tmState, Nothing)
        Just tmWin -> do
          let newWins = deleteIdMap tmWinId (tmStateWindows tmState)
          pure (tmState {tmStateWindows = newWins}, Just tmWin)
  case removed of
    Nothing -> pure ()
    Just _ -> do
      TMState{tmStateApp, tmStateWindows} <- readMVar mvarTMState
      when (null (keysIdMap tmStateWindows)) $ applicationQuit tmStateApp
  pure removed

-- | Ask the user whether to close a window that still has terminals running.
--
-- If the 'confirmExit' option is disabled, this returns 'True' without
-- prompting.
askShouldCloseWindow :: TMState -> ApplicationWindow -> IO Bool
askShouldCloseWindow mvarTMState win = do
  tmState <- readMVar mvarTMState
  let confirm = tmState ^. lensTMStateConfig . lensOptions . lensConfirmExit
  if not confirm
    then pure True
    else do
      dialog <- dialogNew
      box <- dialogGetContentArea dialog
      label <-
        labelNew $
          Just
            "There are still terminals running in this window.  Are you sure you want to close it?"
      containerAdd box label
      widgetShow label
      setWidgetMargin label 10
      void $
        dialogAddButton
          dialog
          "No, do NOT close this window"
          (fromIntegral (fromEnum ResponseTypeNo))
      void $
        dialogAddButton
          dialog
          "Yes, close this window"
          (fromIntegral (fromEnum ResponseTypeYes))
      windowSetTransientFor dialog (Just win)
      res <- dialogRun dialog
      widgetDestroy dialog
      pure $ toEnum (fromIntegral res) == ResponseTypeYes

-- | Create a new Termonad window: build its GTK 'ApplicationWindow' and
-- 'Notebook', register it in the 'TMState', open an initial terminal in it,
-- connect the per-window callbacks, and present it.
--
-- Returns the id of the newly-created window.
newWindow :: TMState -> IO TMWindowId
newWindow mvarTMState = do
  assertInvariantTMState mvarTMState

  TMState{tmStateApp, tmStateConfig} <- readMVar mvarTMState

  -- Build the GTK window and its notebook from the interface description.
  uiBuilder <-
    builderNewFromString interfaceText $ fromIntegral (Text.length interfaceText)
  builderSetApplication uiBuilder tmStateApp
  appWin <- objFromBuildUnsafe uiBuilder "appWin" ApplicationWindow
  applicationAddWindow tmStateApp appWin

  box <- objFromBuildUnsafe uiBuilder "content_box" Box
  note <- notebookNew
  widgetSetCanFocus note False
  -- If this is not set to False, then there will be a one pixel white border
  -- shown around the notebook.
  notebookSetShowBorder note False
  boxPackStart box note True True 0

  -- Register the new (still empty) window in the state and get its id.
  tmWinId <-
    modifyMVar mvarTMState $ \tmState -> do
      let tmNote = createEmptyTMNotebook note
          tmWin = createTMWindow appWin tmNote
          (newWinId, newWins) = insertIdMap tmWin (tmStateWindows tmState)
      pure (tmState {tmStateWindows = newWins}, newWinId)

  -- Open the first terminal.  This MUST happen before setupWindowCallbacks
  -- connects the switch-page handler: appending the first page to an empty
  -- notebook makes GTK synchronously emit switch-page, and that handler takes
  -- the TMState MVar -- which createTerm/addPage is still holding at that
  -- moment, so connecting it earlier would deadlock.
  terminal <- createTerm handleKeyPress mvarTMState tmWinId

  -- Connect the per-window callbacks now that the first tab exists.
  setupWindowCallbacks mvarTMState tmStateApp appWin note tmWinId

  -- Per-window chrome.
  let showMenu = tmStateConfig ^. lensOptions . lensShowMenu
  applicationWindowSetShowMenubar appWin showMenu
  windowSetTitle appWin "Termonad"

  widgetShowAll appWin
  widgetGrabFocus $ terminal ^. lensTerm
  windowPresent appWin

  assertInvariantTMState mvarTMState
  pure tmWinId
