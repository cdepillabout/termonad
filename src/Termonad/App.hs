{-# LANGUAGE TemplateHaskell #-}

module Termonad.App where

import Termonad.Prelude

import Control.Lens ((^.))
import Data.FileEmbed (embedFile)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import GI.Gdk (screenGetDefault)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GI.Gio
  ( ApplicationFlags(ApplicationFlagsFlagsNone, ApplicationFlagsNonUnique)
  , MenuModel(MenuModel)
  , actionMapAddAction
  , afterApplicationStartup
  , applicationQuit
  , applicationRun
  , onApplicationActivate
  , onSimpleActionActivate
  , simpleActionNew
  )
import GI.GLib (setApplicationName, setPrgname)
import GI.Gtk
  ( Application
  , ResponseType(ResponseTypeNo, ResponseTypeYes)
  , pattern STYLE_PROVIDER_PRIORITY_APPLICATION
  , applicationGetActiveWindow
  , applicationSetAccelsForAction
  , applicationSetMenubar
  , builderNewFromString
  , containerAdd
  , cssProviderLoadFromData
  , cssProviderNew
  , dialogAddButton
  , dialogGetContentArea
  , dialogNew
  , dialogRun
  , labelNew
  , setWidgetMargin
  , styleContextAddProviderForScreen
  , widgetDestroy
  , widgetShow
  , windowSetDefaultIcon
  , windowSetTransientFor
  )
import Termonad.Gtk (appNew, imgToPixbuf, objFromBuildUnsafe)
import Termonad.Lenses
  ( lensConfirmExit
  , lensOptions
  , lensTMStateApp
  , lensTMStateConfig
  )
import Termonad.Preferences (showPreferencesDialog)
import Termonad.Types
  ( InstanceMode(SeparateInstance, SingleInstance)
  , TMConfig
  , TMState
  , TMState'
  , createFontDescFromConfig
  , modFontSize
  , newEmptyTMState
  )
import Termonad.XML (menuText)
import Termonad.Window (showAboutDialog, modifyFontSizeForAllTerms, newWindow)

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

-- | Set up the application-scoped GActions (those under the @app.@ prefix).
setupAppCallbacks :: TMState -> Application -> IO ()
setupAppCallbacks mvarTMState app = do
  newWindowAction <- simpleActionNew "newwin" Nothing
  void $ onSimpleActionActivate newWindowAction $ \_ ->
    void $ newWindow mvarTMState
  actionMapAddAction app newWindowAction
  applicationSetAccelsForAction app "app.newwin" ["<Shift><Ctrl>N"]

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
    modifyFontSizeForAllTerms (modFontSize 1) mvarTMState
  actionMapAddAction app enlargeFontAction
  applicationSetAccelsForAction app "app.enlargefont" ["<Ctrl>plus"]

  reduceFontAction <- simpleActionNew "reducefont" Nothing
  void $ onSimpleActionActivate reduceFontAction $ \_ ->
    modifyFontSizeForAllTerms (modFontSize (-1)) mvarTMState
  actionMapAddAction app reduceFontAction
  applicationSetAccelsForAction app "app.reducefont" ["<Ctrl>minus"]

  aboutAction <- simpleActionNew "about" Nothing
  void $ onSimpleActionActivate aboutAction $ \_ -> showAboutDialog app
  actionMapAddAction app aboutAction

-- | One-time, per-primary-instance setup.
--
-- GApplication emits @startup@ exactly once for the primary instance, before
-- the first @activate@ (and before any second invocation can activate us), so
-- this is where all the process-global state lives: the window icon, the
-- screen CSS, the shared 'TMState', the app-wide menu bar, and the @app.@
-- GActions.  The resulting 'TMState' is stashed in the 'IORef' so that every
-- 'appActivate' -- the first one and each subsequent one -- can open a window
-- against it.
--
-- This is connected with 'afterApplicationStartup' rather than
-- 'onApplicationStartup': GtkApplication only runs @gtk_init@ in the default
-- @startup@ handler, which (being a @RUN_LAST@ signal) fires after ordinary
-- @connect@ handlers.  Anything display-dependent (the icon, 'setupScreenStyle')
-- must therefore run in an @after@ handler.
appStartup :: TMConfig -> Application -> IORef (Maybe TMState) -> IO ()
appStartup tmConfig app tmStateRef = do
  let img = $(embedFile "img/termonad-lambda.png")
  iconPixbuf <- imgToPixbuf img
  windowSetDefaultIcon iconPixbuf
  setupScreenStyle
  fontDesc <- createFontDescFromConfig tmConfig
  mvarTMState <- newEmptyTMState tmConfig app fontDesc
  -- TODO: This IORef is really ugly.  I'd like to rearchitect this so we don't need it...
  writeIORef tmStateRef (Just mvarTMState)

  -- Set up the application-wide menu bar model.
  menuBuilder <- builderNewFromString menuText $ fromIntegral (Text.length menuText)
  menuModel <- objFromBuildUnsafe menuBuilder "menubar" MenuModel
  applicationSetMenubar app (Just menuModel)

  setupAppCallbacks mvarTMState app

-- | Open a new window.  This runs on every activation: the initial launch and,
-- when Termonad is a single instance, each later @termonad@ invocation that
-- activates the already-running primary instead of starting a new process.
appActivate :: IORef (Maybe TMState) -> IO ()
appActivate tmStateRef = do
  maybeTMState <- readIORef tmStateRef
  case maybeTMState of
    -- 'appStartup' always runs before the first 'appActivate', so by the time
    -- we get here the IORef has been populated.
    Nothing -> fail "appActivate: TMState was not initialized in appStartup"
    Just mvarTMState -> void $ newWindow mvarTMState

-- | Run Termonad with the given 'TMConfig'.
--
-- Termonad registers as a single-instance GApplication, so launching @termonad@
-- while one is already running opens a new window in the existing process
-- rather than starting a second one.  Use 'startWith' with 'SeparateInstance'
-- (the @--no-remote@ CLI flag) to force a fully separate process.
--
-- Do not perform any of the recompilation operations that the 'Termonad.Startup.defaultMain'
-- function does.
--
-- This function __does not__ parse command line arguments.
start :: TMConfig -> IO ()
start = startWith SingleInstance

-- | Like 'start', but the 'InstanceMode' selects GApplication uniqueness.
--
-- With 'SingleInstance' (what 'start' uses) a second @termonad@ invocation
-- activates the running primary, which opens a new window.  With
-- 'SeparateInstance' (the @--no-remote@ flag) this process registers as
-- non-unique, so it gets its own state and windows and neither contacts nor
-- becomes the primary instance.
startWith :: InstanceMode -> TMConfig -> IO ()
startWith instanceMode tmConfig = do
  -- Set explicit GLib program and application names.  On macOS (where Termonad
  -- runs as a native Quartz app) this makes the menu bar show "Termonad" rather
  -- than the underlying (possibly dyre-recompiled) executable name.  The Dock
  -- name and icon instead come from the generated Termonad.app bundle.
  setPrgname "termonad"
  setApplicationName "Termonad"
  -- Register a stable application id so GApplication can enforce single-instance
  -- behavior.  --no-remote adds the NonUnique flag, which tells GApplication to
  -- skip the single-instance negotiation entirely (own primary, own state).
  let appFlags =
        case instanceMode of
          SingleInstance -> [ApplicationFlagsFlagsNone]
          SeparateInstance -> [ApplicationFlagsNonUnique]
  app <- appNew (Just "com.github.cdepillabout.termonad") appFlags
  tmStateRef <- newIORef Nothing
  void $ afterApplicationStartup app (appStartup tmConfig app tmStateRef)
  void $ onApplicationActivate app (appActivate tmStateRef)
  void $ applicationRun app Nothing
