
module Termonad.App where

import Termonad.Prelude

import Config.Dyre (defaultParams, projectName, realMain, showError, wrapMain)
import Control.Lens ((&), (^.), (.~))
import qualified GI.Gdk as Gdk
import GI.Gdk (AttrOp((:=)), new, screenGetDefault)
import GI.Gio
  ( ApplicationFlags(ApplicationFlagsFlagsNone)
  , MenuModel(MenuModel)
  , actionMapAddAction
  , applicationRun
  , onSimpleActionActivate
  , simpleActionNew
  )
import GI.Gtk
  ( Application
  , ApplicationWindow(ApplicationWindow)
  , Box(Box)
  , CssProvider(CssProvider)
  , Notebook(Notebook)
  , pattern STYLE_PROVIDER_PRIORITY_APPLICATION
  , aboutDialogNew
  , applicationAddWindow
  , applicationGetActiveWindow
  , applicationNew
  , applicationSetAccelsForAction
  , applicationSetMenubar
  , builderNewFromString
  , builderSetApplication
  , dialogRun
  , onNotebookSwitchPage
  , styleContextAddProviderForScreen
  , widgetDestroy
  , widgetGrabFocus
  , widgetShowAll
  , windowPresent
  , windowSetTransientFor
  )
import qualified GI.Gtk as Gtk
import GI.Pango
  ( FontDescription
  , pattern SCALE
  , fontDescriptionNew
  , fontDescriptionSetFamily
  , fontDescriptionSetSize
  )
import GI.Vte
  ( terminalCopyClipboard
  , terminalPasteClipboard
  )

import Termonad.Config
  ( FontConfig(fontFamily, fontSize)
  , TMConfig
  , lensFontConfig
  )
import Termonad.FocusList (updateFocusFL)
import Termonad.Gtk (objFromBuildUnsafe)
import Termonad.Keys (handleKeyPress)
import Termonad.Term (createTerm, termExitFocused)
import Termonad.Types
  ( getFocusedTermFromState
  , lensTMNotebookTabs
  , lensTMNotebookTabTerm
  , lensTMStateNotebook
  , lensTerm
  , newEmptyTMState
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
      cssProvider <- new CssProvider []
      let (textLines :: [Text]) =
            [
              "scrollbar {"
            -- , "  -GtkRange-slider-width: 200px;"
            -- , "  -GtkRange-stepper-size: 200px;"
            -- , "  border-width: 200px;"
            , "  background-color: #aaaaaa;"
            -- , "  color: #ff0000;"
            , "  min-width: 4px;"
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
      #loadFromData cssProvider styleData
      styleContextAddProviderForScreen
        screen
        cssProvider
        (fromIntegral STYLE_PROVIDER_PRIORITY_APPLICATION)

createFontDesc :: TMConfig -> IO FontDescription
createFontDesc tmConfig = do
  fontDesc <- fontDescriptionNew
  let fontConf = tmConfig ^. lensFontConfig
  fontDescriptionSetFamily fontDesc (fontFamily fontConf)
  fontDescriptionSetSize fontDesc (fromIntegral (fontSize fontConf) * SCALE)
  pure fontDesc

setupTermonad :: TMConfig -> Application -> ApplicationWindow -> Gtk.Builder -> IO ()
setupTermonad tmConfig app win builder = do
  void $ Gdk.on win #destroy (#quit app)
  setupScreenStyle
  box <- objFromBuildUnsafe builder "content_box" Box
  fontDesc <- createFontDesc tmConfig
  note <- new Notebook [#canFocus := False]
  #packStart box note True True 0

  void $ Gdk.on note #pageRemoved $ \_ _ -> do
    pages <- #getNPages note
    when (pages == 0) (#quit app)

  mvarTMState <- newEmptyTMState tmConfig app win note fontDesc
  terminal <- createTerm handleKeyPress mvarTMState

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


  newTabAction <- simpleActionNew "newtab" Nothing
  void $ onSimpleActionActivate newTabAction $ \_ -> void $ createTerm handleKeyPress mvarTMState
  actionMapAddAction app newTabAction
  applicationSetAccelsForAction app "app.newtab" ["<Shift><Ctrl>T"]

  closeTabAction <- simpleActionNew "closetab" Nothing
  void $ onSimpleActionActivate closeTabAction $ \_ -> termExitFocused mvarTMState
  actionMapAddAction app closeTabAction
  applicationSetAccelsForAction app "app.closetab" ["<Shift><Ctrl>W"]

  quitAction <- simpleActionNew "quit" Nothing
  void $ onSimpleActionActivate quitAction $ \_ -> do
    putStrLn "got quit!"
    withMVar mvarTMState (\tmState -> putStrLn $ "tmState: " <> tshow tmState)
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

  menuBuilder <- builderNewFromString menuText $ fromIntegral (length menuText)
  menuModel <- objFromBuildUnsafe menuBuilder "menubar" MenuModel
  applicationSetMenubar app (Just menuModel)

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
  windowSetTransientFor aboutDialog (Just win)
  void $ dialogRun aboutDialog
  widgetDestroy aboutDialog

appStartup :: Application -> IO ()
appStartup _app = pure ()

start :: TMConfig -> IO ()
start tmConfig = do
  maybeApp <- applicationNew (Just "haskell.termonad") [ApplicationFlagsFlagsNone]
  case maybeApp of
    Nothing -> error "Could not create application for some reason!"
    Just app -> do
      void $ Gdk.on app #startup (appStartup app)
      void $ Gdk.on app #activate (appActivate tmConfig app)
      void $ applicationRun app Nothing

defaultMain :: TMConfig -> IO ()
defaultMain tmConfig = do
  let params =
        defaultParams
          { projectName = "termonad"
          , showError = \(cfg, oldErrs) newErr -> (cfg, oldErrs <> "\n" <> newErr)
          , realMain = \(cfg, errs) -> putStrLn (pack errs) *> start cfg
          }
  wrapMain params (tmConfig, "")
