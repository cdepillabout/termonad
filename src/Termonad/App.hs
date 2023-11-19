{-# LANGUAGE TemplateHaskell #-}

module Termonad.App where

import Termonad.Prelude

import Control.Lens ((^.))
import Data.FileEmbed (embedFile)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import GI.Gdk (screenGetDefault)
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
  , Notebook
  , ResponseType(ResponseTypeNo, ResponseTypeYes)
  , pattern STYLE_PROVIDER_PRIORITY_APPLICATION
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
  , dialogRun
  , labelNew
  , notebookGetNPages
  , notebookNew
  , notebookSetShowBorder
  , onNotebookPageRemoved
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
import Termonad.Gtk (appNew, imgToPixbuf, objFromBuildUnsafe)
import Termonad.Keys (handleKeyPress)
import Termonad.Lenses
  ( lensConfirmExit
  , lensOptions
  , lensShowMenu
  , lensTMStateApp
  , lensTMStateConfig
  , lensTerm
  )
import Termonad.Preferences (showPreferencesDialog)
import Termonad.Term (createTerm, setShowTabs)
import Termonad.Types
  ( TMConfig
  , TMState
  , TMState'
  , TMWindowId
  , createFontDescFromConfig
  , modFontSize
  , newEmptyTMState
  )
import Termonad.XML (interfaceText, menuText)
import Termonad.Window (showAboutDialog, modifyFontSizeForAllTerms, setupWindowCallbacks)

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
      let styleData = encodeUtf8 (Text.unlines textLines)
      cssProviderLoadFromData cssProvider styleData
      styleContextAddProviderForScreen
        screen
        cssProvider
        (fromIntegral STYLE_PROVIDER_PRIORITY_APPLICATION)

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
{- HLINT ignore "Reduce duplication" -}
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

setupAppCallbacks :: TMState -> TMConfig -> Application -> ApplicationWindow -> Notebook -> TMWindowId -> IO ()
setupAppCallbacks mvarTMState tmConfig app win note tmWinId = do
  newWindowAction <- simpleActionNew "newwin" Nothing
  void $ onSimpleActionActivate newWindowAction $ \_ ->
    pure ()
    -- void $ createTerm handleKeyPress mvarTMState tmWinId
  actionMapAddAction app newWindowAction
  applicationSetAccelsForAction app "app.newwin" ["<Shift><Ctrl>N"]

  void $ onNotebookPageRemoved note $ \_ _ -> do
    pages <- notebookGetNPages note
    if pages == 0
      then forceQuit mvarTMState
      else setShowTabs tmConfig note

  quitAction <- simpleActionNew "quit" Nothing
  void $ onSimpleActionActivate quitAction $ \_ -> do
    shouldExit <- askShouldExit mvarTMState
    when (shouldExit == ResponseTypeYes) $ forceQuit mvarTMState
  actionMapAddAction app quitAction
  applicationSetAccelsForAction app "app.quit" ["<Shift><Ctrl>Q"]

  preferencesAction <- simpleActionNew "preferences" Nothing
  void $ onSimpleActionActivate preferencesAction (const $ showPreferencesDialog mvarTMState)
  actionMapAddAction app preferencesAction

  enlargeFontAction <- simpleActionNew "enlargefont" Nothing
  void $ onSimpleActionActivate enlargeFontAction $ \_ ->
    modifyFontSizeForAllTerms (modFontSize 1) mvarTMState tmWinId
  actionMapAddAction app enlargeFontAction
  applicationSetAccelsForAction app "app.enlargefont" ["<Ctrl>plus"]

  reduceFontAction <- simpleActionNew "reducefont" Nothing
  void $ onSimpleActionActivate reduceFontAction $ \_ ->
    modifyFontSizeForAllTerms (modFontSize (-1)) mvarTMState tmWinId
  actionMapAddAction app reduceFontAction
  applicationSetAccelsForAction app "app.reducefont" ["<Ctrl>minus"]

  aboutAction <- simpleActionNew "about" Nothing
  void $ onSimpleActionActivate aboutAction $ \_ -> showAboutDialog win
  actionMapAddAction app aboutAction

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

setupTermonad :: TMConfig -> Application -> ApplicationWindow -> Gtk.Builder -> IO ()
setupTermonad tmConfig app win builder = do
  setupScreenStyle
  box <- objFromBuildUnsafe builder "content_box" Box
  fontDesc <- createFontDescFromConfig tmConfig
  note <- notebookNew
  widgetSetCanFocus note False
  -- If this is not set to False, then there will be a one pixel white border
  -- shown around the notebook.
  notebookSetShowBorder note False
  boxPackStart box note True True 0

  (mvarTMState, tmWinId) <- newEmptyTMState tmConfig app win note fontDesc
  terminal <- createTerm handleKeyPress mvarTMState tmWinId

  setupAppCallbacks mvarTMState tmConfig app win note tmWinId
  setupWindowCallbacks mvarTMState app win note tmWinId

  menuBuilder <- builderNewFromString menuText $ fromIntegral (Text.length menuText)
  menuModel <- objFromBuildUnsafe menuBuilder "menubar" MenuModel
  applicationSetMenubar app (Just menuModel)
  let showMenu = tmConfig ^. lensOptions . lensShowMenu
  applicationWindowSetShowMenubar win showMenu

  windowSetTitle win "Termonad"

  widgetShowAll win
  widgetGrabFocus $ terminal ^. lensTerm

appActivate :: TMConfig -> Application -> IO ()
appActivate tmConfig app = do
  let img = $(embedFile "img/termonad-lambda.png")
  iconPixbuf <- imgToPixbuf img
  windowSetDefaultIcon iconPixbuf
  uiBuilder <-
    builderNewFromString interfaceText $ fromIntegral (Text.length interfaceText)
  builderSetApplication uiBuilder app
  appWin <- objFromBuildUnsafe uiBuilder "appWin" ApplicationWindow
  applicationAddWindow app appWin
  setupTermonad tmConfig app appWin uiBuilder
  windowPresent appWin

appStartup :: Application -> IO ()
appStartup _app = pure ()

-- | Run Termonad with the given 'TMConfig'.
--
-- Do not perform any of the recompilation operations that the 'Termonad.Startup.defaultMain'
-- function does.
--
-- This function __does not__ parse command line arguments.
start :: TMConfig -> IO ()
start tmConfig = do
  -- app <- appNew (Just "haskell.termonad") [ApplicationFlagsFlagsNone]
  -- Make sure the application is not unique, so we can open multiple copies of it.
  app <- appNew Nothing [ApplicationFlagsFlagsNone]
  void $ onApplicationStartup app (appStartup app)
  void $ onApplicationActivate app (appActivate tmConfig app)
  void $ applicationRun app Nothing
