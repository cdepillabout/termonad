
module Termonad.Window where

import Termonad.Prelude

import Control.Lens ((^.), (^..), set, view, ix)
import Data.FocusList (focusList, moveFromToFL, updateFocusFL)
import Data.Sequence (findIndexR)
import qualified Data.Text as Text
import GI.Gdk (castTo, managedForeignPtr)
import GI.Gio
  ( actionMapAddAction
  , onSimpleActionActivate
  , simpleActionNew
  )
import GI.Gtk
  ( Application
  , ApplicationWindow
  , Notebook
  , PositionType(PositionTypeRight)
  , ResponseType(ResponseTypeNo, ResponseTypeYes)
  , ScrolledWindow(ScrolledWindow)
  , Widget
  , aboutDialogNew
  , applicationSetAccelsForAction
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
  , onEntryActivate
  , onNotebookPageReordered
  , onNotebookSwitchPage
  , setWidgetMargin
  , widgetDestroy
  , widgetGrabFocus
  , widgetShow
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
import Termonad.Lenses
  ( lensTMNotebookTabs
  , lensTMNotebookTabTerm
  , lensTMStateFontDesc
  , lensTerm
  , lensTMStateWindows
  , lensTMWindowNotebook
  )
import Termonad.Term
  ( createTerm
  , relabelTabs
  , termNextPage
  , termPrevPage
  , termExitFocused
  )
import Termonad.Types
  ( FontSize
  , TMNotebookTab
  , TMState
  , TMWindowId
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

modifyFontSizeForAllTerms :: (FontSize -> FontSize) -> TMState -> TMWindowId -> IO ()
modifyFontSizeForAllTerms modFontSizeFunc mvarTMState tmWinId = do
  tmState <- readMVar mvarTMState
  let fontDesc = tmState ^. lensTMStateFontDesc
  adjustFontDescSize modFontSizeFunc fontDesc
  let terms =
        tmState ^..
          lensTMStateWindows .
          ix tmWinId .
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
  maybeScrollWin <- castTo ScrolledWindow childWidg
  case maybeScrollWin of
    Nothing ->
      fail $
        "In setupTermonad, in callback for onNotebookPageReordered, " <>
        "child widget is not a ScrolledWindow.\n" <>
        "Don't know how to continue.\n"
    Just scrollWin -> do
      tmNote <- getTMNotebookFromTMState mvarTMState tmWinId
      let fl = view lensTMNotebookTabs tmNote
      let maybeOldPosition =
            findIndexR (compareScrolledWinAndTab scrollWin) (focusList fl)
      case maybeOldPosition of
        Nothing ->
          fail $
            "In setupTermonad, in callback for onNotebookPageReordered, " <>
            "the ScrolledWindow is not already in the FocusList.\n" <>
            "Don't know how to continue.\n"
        Just oldPos -> do
          updateFLTabPos mvarTMState tmWinId oldPos (fromIntegral pageNum)
          tmNote' <- getTMNotebookFromTMState mvarTMState tmWinId
          relabelTabs tmNote'
  where
    compareScrolledWinAndTab :: ScrolledWindow -> TMNotebookTab -> Bool
    compareScrolledWinAndTab scrollWin flTab =
      let ScrolledWindow managedPtrFLTab = tmNotebookTabTermContainer flTab
          foreignPtrFLTab = managedForeignPtr managedPtrFLTab
          ScrolledWindow managedPtrScrollWin = scrollWin
          foreignPtrScrollWin = managedForeignPtr managedPtrScrollWin
      in foreignPtrFLTab == foreignPtrScrollWin

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

showAboutDialog :: ApplicationWindow -> IO ()
showAboutDialog win = do
  aboutDialog <- aboutDialogNew
  windowSetTransientFor aboutDialog (Just win)
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
