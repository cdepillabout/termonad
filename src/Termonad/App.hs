
module Termonad.App where

import Termonad.Prelude

import Config.Dyre (defaultParams, projectName, realMain, showError, wrapMain)
import Control.Lens ((&), (.~), (^.))
import Data.FocusList (focusList, moveFromToFL, updateFocusFL)
import Data.Sequence (findIndexR)
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
  , ResponseType(ResponseTypeNo, ResponseTypeYes)
  , ScrolledWindow(ScrolledWindow)
  , pattern STYLE_PROVIDER_PRIORITY_APPLICATION
  , aboutDialogNew
  , applicationAddWindow
  , applicationGetActiveWindow
  , applicationSetAccelsForAction
  , applicationSetMenubar
  , boxPackStart
  , builderNewFromString
  , builderSetApplication
  , containerAdd
  , cssProviderLoadFromData
  , cssProviderNew
  , dialogAddButton
  , dialogGetContentArea
  , dialogNew
  , dialogRun
  , labelNew
  , notebookGetNPages
  , notebookNew
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
  , windowSetDefaultIconFromFile
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
  ( terminalCopyClipboard
  , terminalPasteClipboard
  )

import Paths_termonad (getDataFileName)
import Termonad.Lenses
  ( lensConfirmExit
  , lensFontConfig
  , lensOptions
  , lensShowMenu
  , lensTMNotebookTabTerm
  , lensTMNotebookTabs
  , lensTMStateApp
  , lensTMStateConfig
  , lensTMStateNotebook
  , lensTerm
  )
import Termonad.Gtk (appNew, objFromBuildUnsafe)
import Termonad.Keys (handleKeyPress)
import Termonad.Term (createTerm, relabelTabs, termExitFocused, setShowTabs)
import Termonad.Types
  ( FontConfig(fontFamily, fontSize)
  , FontSize(FontSizePoints, FontSizeUnits)
  , TMConfig
  , TMNotebookTab
  , TMState
  , TMState'(TMState)
  , getFocusedTermFromState
  , newEmptyTMState
  , tmNotebookTabTermContainer
  , tmNotebookTabs
  , tmStateNotebook
  )
import Termonad.XML (interfaceText, menuText)

setupScreenStyle :: IO ()
setupScreenStyle = do
  maybeScreen <- screenGetDefault
  case maybeScreen of
    Nothing -> pure ()
    Just screen -> do
      cssProvider <- cssProviderNew
      let (textLines :: [Text]) =
            [
              "scrollbar {"
            -- , "  -GtkRange-slider-width: 200px;"
            -- , "  -GtkRange-stepper-size: 200px;"
            -- , "  border-width: 200px;"
            , "  background-color: #aaaaaa;"
            -- , "  color: #ff0000;"
            -- , "  min-width: 4px;"
            , "}"
            -- , "scrollbar trough {"
            -- , "  -GtkRange-slider-width: 200px;"
            -- , "  -GtkRange-stepper-size: 200px;"
            -- , "  border-width: 200px;"
            -- , "  background-color: #00ff00;"
            -- , "  color: #00ff00;"
            -- , "  min-width: 50px;"
            -- , "}"
            -- , "scrollbar slider {"
            -- , "  -GtkRange-slider-width: 200px;"
            -- , "  -GtkRange-stepper-size: 200px;"
            -- , "  border-width: 200px;"
            -- , "  background-color: #0000ff;"
            -- , "  color: #0000ff;"
            -- , "  min-width: 50px;"
            -- , "}"
            , "tab {"
            , "  background-color: transparent;"
            , "}"
            ]
      let styleData = encodeUtf8 (unlines textLines :: Text)
      cssProviderLoadFromData cssProvider styleData
      styleContextAddProviderForScreen
        screen
        cssProvider
        (fromIntegral STYLE_PROVIDER_PRIORITY_APPLICATION)

createFontDesc :: TMConfig -> IO FontDescription
createFontDesc tmConfig = do
  fontDesc <- fontDescriptionNew
  let fontConf = tmConfig ^. lensOptions . lensFontConfig
  fontDescriptionSetFamily fontDesc (fontFamily fontConf)
  case fontSize fontConf of
    FontSizePoints points ->
      fontDescriptionSetSize fontDesc $ fromIntegral (points * fromIntegral SCALE)
    FontSizeUnits units ->
      fontDescriptionSetAbsoluteSize fontDesc $ units * fromIntegral SCALE
  pure fontDesc

compareScrolledWinAndTab :: ScrolledWindow -> TMNotebookTab -> Bool
compareScrolledWinAndTab scrollWin flTab =
  let ScrolledWindow managedPtrFLTab = tmNotebookTabTermContainer flTab
      foreignPtrFLTab = managedForeignPtr managedPtrFLTab
      ScrolledWindow managedPtrScrollWin = scrollWin
      foreignPtrScrollWin = managedForeignPtr managedPtrScrollWin
  in foreignPtrFLTab == foreignPtrScrollWin

updateFLTabPos :: TMState -> Int -> Int -> IO ()
updateFLTabPos mvarTMState oldPos newPos =
  modifyMVar_ mvarTMState $ \tmState -> do
    let tabs = tmState ^. lensTMStateNotebook . lensTMNotebookTabs
        maybeNewTabs = moveFromToFL oldPos newPos tabs
    case maybeNewTabs of
      Nothing -> do
        putStrLn $
          "in updateFLTabPos, Strange error: couldn't move tabs.\n" <>
          "old pos: " <> tshow oldPos <> "\n" <>
          "new pos: " <> tshow newPos <> "\n" <>
          "tabs: " <> tshow tabs <> "\n" <>
          "maybeNewTabs: " <> tshow maybeNewTabs <> "\n" <>
          "tmState: " <> tshow tmState
        pure tmState
      Just newTabs ->
        pure $
          tmState &
            lensTMStateNotebook . lensTMNotebookTabs .~ newTabs

-- | Try to figure out whether Termonad should exit.  This also used to figure
-- out if Termonad should close a given terminal.
--
-- This reads the 'confirmExit' setting from 'ConfigOptions' to check whether
-- the user wants to be notified when either Termonad or a given terminal is
-- about to be closed.
--
-- If 'confirmExit' is 'True', then a dialog is presented to the user asking
-- them if they really want to exit or close the terminal.  Their response is
-- sent back as a 'ResponseType'.
--
-- If 'confirmExit' is 'False', then this function always returns
-- 'ResponseTypeYes'.
askShouldExit :: TMState -> IO ResponseType
askShouldExit mvarTMState = do
  tmState <- readMVar mvarTMState
  let confirm = tmState ^. lensTMStateConfig . lensOptions . lensConfirmExit
  if confirm
    then confirmationDialogForExit tmState
    else pure ResponseTypeYes
  where
    -- Show the user a dialog telling them there are still terminals running and
    -- asking if they really want to exit.
    --
    -- Return the user's resposne as a 'ResponseType'.
    confirmationDialogForExit :: TMState' -> IO ResponseType
    confirmationDialogForExit tmState = do
      let app = tmState ^. lensTMStateApp
      win <- applicationGetActiveWindow app
      dialog <- dialogNew
      box <- dialogGetContentArea dialog
      label <-
        labelNew $
          Just
            "There are still terminals running.  Are you sure you want to exit?"
      containerAdd box label
      widgetShow label
      setWidgetMargin label 10
      void $
        dialogAddButton
          dialog
          "No, do NOT exit"
          (fromIntegral (fromEnum ResponseTypeNo))
      void $
        dialogAddButton
          dialog
          "Yes, exit"
          (fromIntegral (fromEnum ResponseTypeYes))
      windowSetTransientFor dialog win
      res <- dialogRun dialog
      widgetDestroy dialog
      pure $ toEnum (fromIntegral res)

-- | Force Termonad to exit without asking the user whether or not to do so.
forceQuit :: TMState -> IO ()
forceQuit mvarTMState = do
  tmState <- readMVar mvarTMState
  let app = tmState ^. lensTMStateApp
  applicationQuit app

setupTermonad :: TMConfig -> Application -> ApplicationWindow -> Gtk.Builder -> IO ()
setupTermonad tmConfig app win builder = do
  termonadIconPath <- getDataFileName "img/termonad-lambda.png"
  windowSetDefaultIconFromFile termonadIconPath

  setupScreenStyle
  box <- objFromBuildUnsafe builder "content_box" Box
  fontDesc <- createFontDesc tmConfig
  note <- notebookNew
  widgetSetCanFocus note False
  boxPackStart box note True True 0

  mvarTMState <- newEmptyTMState tmConfig app win note fontDesc
  terminal <- createTerm handleKeyPress mvarTMState

  void $ onNotebookPageRemoved note $ \_ _ -> do
    pages <- notebookGetNPages note
    if pages == 0
      then forceQuit mvarTMState
      else setShowTabs tmConfig note

  void $ onNotebookSwitchPage note $ \_ pageNum -> do
    maybeRes <- tryTakeMVar mvarTMState
    case maybeRes of
      Nothing -> pure ()
      Just val -> do
        putMVar mvarTMState val
    modifyMVar_ mvarTMState $ \tmState -> do
      let notebook = tmStateNotebook tmState
          tabs = tmNotebookTabs notebook
          maybeNewTabs = updateFocusFL (fromIntegral pageNum) tabs
      case maybeNewTabs of
        Nothing -> pure tmState
        Just (tab, newTabs) -> do
          widgetGrabFocus $ tab ^. lensTMNotebookTabTerm . lensTerm
          pure $
            tmState &
              lensTMStateNotebook . lensTMNotebookTabs .~ newTabs

  void $ onNotebookPageReordered note $ \childWidg pageNum -> do
    maybeScrollWin <- castTo ScrolledWindow childWidg
    case maybeScrollWin of
      Nothing ->
        fail $
          "In setupTermonad, in callback for onNotebookPageReordered, " <>
          "child widget is not a ScrolledWindow.\n" <>
          "Don't know how to continue.\n"
      Just scrollWin -> do
        TMState{tmStateNotebook} <- readMVar mvarTMState
        let fl = tmStateNotebook ^. lensTMNotebookTabs
        let maybeOldPosition =
              findIndexR (compareScrolledWinAndTab scrollWin) (focusList fl)
        case maybeOldPosition of
          Nothing ->
            fail $
              "In setupTermonad, in callback for onNotebookPageReordered, " <>
              "the ScrolledWindow is not already in the FocusList.\n" <>
              "Don't know how to continue.\n"
          Just oldPos -> do
            updateFLTabPos mvarTMState oldPos (fromIntegral pageNum)
            relabelTabs mvarTMState

  newTabAction <- simpleActionNew "newtab" Nothing
  void $ onSimpleActionActivate newTabAction $ \_ -> void $ createTerm handleKeyPress mvarTMState
  actionMapAddAction app newTabAction
  applicationSetAccelsForAction app "app.newtab" ["<Shift><Ctrl>T"]

  closeTabAction <- simpleActionNew "closetab" Nothing
  void $ onSimpleActionActivate closeTabAction $ \_ ->
    termExitFocused mvarTMState
  actionMapAddAction app closeTabAction
  applicationSetAccelsForAction app "app.closetab" ["<Shift><Ctrl>W"]

  quitAction <- simpleActionNew "quit" Nothing
  void $ onSimpleActionActivate quitAction $ \_ -> do
    shouldExit <- askShouldExit mvarTMState
    when (shouldExit == ResponseTypeYes) $ forceQuit mvarTMState
  actionMapAddAction app quitAction
  applicationSetAccelsForAction app "app.quit" ["<Shift><Ctrl>Q"]

  copyAction <- simpleActionNew "copy" Nothing
  void $ onSimpleActionActivate copyAction $ \_ -> do
    maybeTerm <- getFocusedTermFromState mvarTMState
    maybe (pure ()) terminalCopyClipboard maybeTerm
  actionMapAddAction app copyAction
  applicationSetAccelsForAction app "app.copy" ["<Shift><Ctrl>C"]

  pasteAction <- simpleActionNew "paste" Nothing
  void $ onSimpleActionActivate pasteAction $ \_ -> do
    maybeTerm <- getFocusedTermFromState mvarTMState
    maybe (pure ()) terminalPasteClipboard maybeTerm
  actionMapAddAction app pasteAction
  applicationSetAccelsForAction app "app.paste" ["<Shift><Ctrl>C"]

  aboutAction <- simpleActionNew "about" Nothing
  void $ onSimpleActionActivate aboutAction (const $ showAboutDialog app)
  actionMapAddAction app aboutAction

  when (tmConfig ^. lensOptions . lensShowMenu) $ do
    menuBuilder <- builderNewFromString menuText $ fromIntegral (length menuText)
    menuModel <- objFromBuildUnsafe menuBuilder "menubar" MenuModel
    applicationSetMenubar app (Just menuModel)

  windowSetTitle win "Termonad"

  -- This event will happen if the user requests that the top-level Termonad
  -- window be closed through their window manager. It will also happen
  -- normally when the user tries to close Termonad through normal methods,
  -- like clicking "Quit" or closing the last open terminal.
  --
  -- If you return 'True' from this callback, then Termonad will not exit.
  -- If you return 'False' from this callback, then Termonad will continue to
  -- exit.
  void $ onWidgetDeleteEvent win $ \_ -> do
    shouldExit <- askShouldExit mvarTMState
    pure $
      case shouldExit of
        ResponseTypeYes -> False
        _ -> True

  widgetShowAll win
  widgetGrabFocus $ terminal ^. lensTerm

appActivate :: TMConfig -> Application -> IO ()
appActivate tmConfig app = do
  uiBuilder <-
    builderNewFromString interfaceText $ fromIntegral (length interfaceText)
  builderSetApplication uiBuilder app
  appWin <- objFromBuildUnsafe uiBuilder "appWin" ApplicationWindow
  applicationAddWindow app appWin
  setupTermonad tmConfig app appWin uiBuilder
  windowPresent appWin

showAboutDialog :: Application -> IO ()
showAboutDialog app = do
  win <- applicationGetActiveWindow app
  aboutDialog <- aboutDialogNew
  windowSetTransientFor aboutDialog win
  void $ dialogRun aboutDialog
  widgetDestroy aboutDialog

appStartup :: Application -> IO ()
appStartup _app = pure ()

-- | Run Termonad with the given 'TMConfig'.
--
-- Do not perform any of the recompilation operations that the 'defaultMain'
-- function does.
start :: TMConfig -> IO ()
start tmConfig = do
  -- app <- appNew (Just "haskell.termonad") [ApplicationFlagsFlagsNone]
  -- Make sure the application is not unique, so we can open multiple copies of it.
  app <- appNew Nothing [ApplicationFlagsFlagsNone]
  void $ onApplicationStartup app (appStartup app)
  void $ onApplicationActivate app (appActivate tmConfig app)
  void $ applicationRun app Nothing

-- | Run Termonad with the given 'TMConfig'.
--
-- This function will check if there is a @~\/.config\/termonad\/termonad.hs@ file
-- and a @~\/.cache\/termonad\/termonad-linux-x86_64@ binary.  Termonad will
-- perform different actions based on whether or not these two files exist.
--
-- Here are the four different possible actions based on the existence of these
-- two files.
--
-- - @~\/.config\/termonad\/termonad.hs@ exists, @~\/.cache\/termonad\/termonad-linux-x86_64@ exists
--
--     The timestamps of these two files are checked.  If the
--     @~\/.config\/termonad\/termonad.hs@ file has been modified after the
--     @~\/.cache\/termonad\/termonad-linux-x86_64@ binary, then Termonad will use
--     GHC to recompile the @~\/.config\/termonad\/termonad.hs@ file, producing a
--     new binary at @~\/.cache\/termonad\/termonad-linux-x86_64@.  This new binary
--     will be re-executed.  The 'TMConfig' passed to this 'defaultMain' will be
--     effectively thrown away.
--
--     If GHC fails to recompile the @~\/.config\/termonad\/termonad.hs@ file, then
--     Termonad will just execute 'start' with the 'TMConfig' passed in.
--
--     If the @~\/.cache\/termonad\/termonad-linux-x86_64@ binary has been modified
--     after the @~\/.config\/termonad\/termonad.hs@ file, then Termonad will
--     re-exec the @~\/.cache\/termonad\/termonad-linux-x86_64@ binary.  The
--     'TMConfig' passed to this 'defaultMain' will be effectively thrown away.
--
-- - @~\/.config\/termonad\/termonad.hs@ exists, @~\/.cache\/termonad\/termonad-linux-x86_64@ does not exist
--
--     Termonad will use GHC to recompile the @~\/.config\/termonad\/termonad.hs@
--     file, producing a new binary at @~\/.cache\/termonad\/termonad-linux-x86_64@.
--     This new binary will be re-executed.  The 'TMConfig' passed to this
--     'defaultMain' will be effectively thrown away.
--
--     If GHC fails to recompile the @~\/.config\/termonad\/termonad.hs@ file, then
--     Termonad will just execute 'start' with the 'TMConfig' passed in.
--
-- - @~\/.config\/termonad\/termonad.hs@ does not exist, @~\/.cache\/termonad\/termonad-linux-x86_64@ exists
--
--     Termonad will ignore the @~\/.cache\/termonad\/termonad-linux-x86_64@ binary
--     and just run 'start' with the 'TMConfig' passed to this function.
--
-- - @~\/.config\/termonad\/termonad.hs@ does not exist, @~\/.cache\/termonad\/termonad-linux-x86_64@ does not exist
--
--     Termonad will run 'start' with the 'TMConfig' passed to this function.
--
-- Other notes:
--
-- 1. That the locations of @~\/.config\/termonad\/termonad.hs@ and
--    @~\/.cache\/termonad\/termonad-linux-x86_64@ may differ depending on your
--    system.
--
-- 2. In your own @~\/.config\/termonad\/termonad.hs@ file, you can use either
--    'defaultMain' or 'start'.  As long as you always execute the system-wide
--    @termonad@ binary (instead of the binary produced as
--    @~\/.cache\/termonad\/termonad-linux-x86_64@), the effect should be the same.
defaultMain :: TMConfig -> IO ()
defaultMain tmConfig = do
  let params =
        defaultParams
          { projectName = "termonad"
          , showError = \(cfg, oldErrs) newErr -> (cfg, oldErrs <> "\n" <> newErr)
          , realMain = \(cfg, errs) -> putStrLn (pack errs) *> start cfg
          }
  eitherRes <- tryIOError $ wrapMain params (tmConfig, "")
  case eitherRes of
    Left ioErr
      | ioeGetErrorType ioErr == doesNotExistErrorType && ioeGetFileName ioErr == Just "ghc" -> do
          putStrLn $
            "Could not find ghc on your PATH.  Ignoring your termonad.hs " <>
            "configuration file and running termonad with default settings."
          start tmConfig
      | otherwise -> do
          putStrLn $ "IO error occurred when trying to run termonad:"
          print ioErr
          putStrLn "Don't know how to recover.  Exiting."
    Right _ -> pure ()
