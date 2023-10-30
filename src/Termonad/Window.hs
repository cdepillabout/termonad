
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
  , windowSetTransientFor
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
