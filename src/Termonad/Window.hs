
module Termonad.Window where

import Termonad.Prelude

import Control.Lens ((^.), (^..), set, view, ix)
import Data.FileEmbed (embedFile)
import Data.FocusList (focusList, moveFromToFL, updateFocusFL)
import Data.Sequence (findIndexR)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import GI.Gdk (castTo, managedForeignPtr, screenGetDefault)
import GI.Gio
  ( ApplicationFlags(ApplicationFlagsFlagsNone)
  , MenuModel(MenuModel)
  , actionMapAddAction
  , applicationQuit
  , applicationRun
  , onApplicationActivate
  , onApplicationStartup
  , onSimpleActionActivate
  , simpleActionNew
  )
import GI.Gtk
  ( Application
  , ApplicationWindow(ApplicationWindow)
  , Box(Box)
  , PositionType(PositionTypeRight)
  , ResponseType(ResponseTypeNo, ResponseTypeYes)
  , ScrolledWindow(ScrolledWindow)
  , pattern STYLE_PROVIDER_PRIORITY_APPLICATION
  , aboutDialogNew
  , applicationAddWindow
  , applicationGetActiveWindow
  , applicationSetAccelsForAction
  , applicationSetMenubar
  , applicationWindowSetShowMenubar
  , boxPackStart
  , builderNewFromString
  , builderSetApplication
  , containerAdd
  , cssProviderLoadFromData
  , cssProviderNew
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
  , styleContextAddProviderForScreen
  , widgetDestroy
  , widgetGrabFocus
  , widgetSetCanFocus
  , widgetShow
  , widgetShowAll
  , windowPresent
  , windowSetDefaultIcon
  , windowSetTitle
  , windowSetTransientFor, Widget
  )
import qualified GI.Gtk as Gtk
import GI.Pango
  ( FontDescription
  , pattern SCALE
  , fontDescriptionNew
  , fontDescriptionSetFamily
  , fontDescriptionSetSize
  , fontDescriptionSetAbsoluteSize
  )
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
import Termonad.Gtk (appNew, imgToPixbuf, objFromBuildUnsafe)
import Termonad.Keys (handleKeyPress)
import Termonad.Lenses
  ( lensConfirmExit
  , lensFontConfig
  , lensOptions
  , lensShowMenu
  , lensTMNotebookTabs
  , lensTMNotebookTabTerm
  , lensTMStateApp
  , lensTMStateConfig
  , lensTMStateFontDesc
  , lensTerm
  , lensTMStateWindows, lensTMWindowNotebook
  )
import Termonad.Preferences (showPreferencesDialog)
import Termonad.Term
  ( createTerm
  , relabelTabs
  , termNextPage
  , termPrevPage
  , termExitFocused
  , setShowTabs
  )
import Termonad.Types
  ( FontConfig(..)
  , FontSize(FontSizePoints, FontSizeUnits)
  , TMConfig
  , TMNotebookTab
  , TMState
  , TMState'
  , TMWindowId
  , fontSizeFromFontDescription
  , getFocusedTermFromState
  , getTMNotebookFromTMState
  , getTMNotebookFromTMState'
  , modFontSize
  , newEmptyTMState
  , tmNotebookTabTermContainer
  , tmNotebookTabs
  , tmStateApp, getTMWindowFromTMState, tmWindowAppWin
  )
import Termonad.XML (interfaceText, menuText)

notebookPageReorderedCallback :: TMState -> TMWindowId -> Widget -> Word32 -> IO ()
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
