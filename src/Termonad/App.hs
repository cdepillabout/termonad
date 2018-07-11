
module Termonad.App where

import Termonad.Prelude

import Control.Lens ((&), (.~), imap)
import Data.Default (def)
import Data.Unique (Unique, newUnique)
import qualified GI.Gdk as Gdk
import GI.Gdk
  ( AttrOp((:=))
  , EventKey
  , GObject
  , pattern KEY_1
  , pattern KEY_2
  , pattern KEY_3
  , pattern KEY_4
  , pattern KEY_5
  , pattern KEY_6
  , pattern KEY_7
  , pattern KEY_8
  , pattern KEY_9
  , pattern KEY_T
  , ManagedPtr
  , ModifierType(..)
  , castTo
  , get
  , new
  , screenGetDefault
  )
import GI.Gio
  ( ApplicationFlags(ApplicationFlagsFlagsNone)
  , MenuModel(MenuModel)
  , actionMapAddAction
  , applicationRun
  , noCancellable
  , onSimpleActionActivate
  , simpleActionNew
  )
import GI.GLib.Flags (SpawnFlags(..))
import GI.Gtk
  ( Application
  , ApplicationWindow(ApplicationWindow)
  , Box(Box)
  , CssProvider(CssProvider)
  , Dialog(Dialog)
  , Notebook(Notebook)
  , ScrolledWindow(ScrolledWindow)
  , pattern STYLE_PROVIDER_PRIORITY_APPLICATION
  , applicationNew
  , applicationSetAccelsForAction
  , builderNewFromString
  , builderSetApplication
  , noWidget
  , onNotebookSwitchPage
  , styleContextAddProviderForScreen
  )
import qualified GI.Gtk as Gtk
import GI.Pango
  ( FontDescription
  , pattern SCALE
  , fontDescriptionNew
  , fontDescriptionSetFamily
  , fontDescriptionSetSize
  )
import GI.Vte (CursorBlinkMode(..), PtyFlags(..), Terminal(Terminal))
import Text.XML (renderText)
import Text.XML.QQ (Document, xmlRaw)

import Termonad.FocusList (setFocusFL)
import Termonad.Gtk
import Termonad.Keys
import Termonad.Term
import Termonad.Types
import Termonad.XML

setupScreenStyle :: IO ()
setupScreenStyle = do
  maybeScreen <- screenGetDefault
  case maybeScreen of
    Nothing -> pure ()
    Just screen -> do
      cssProvider <- new CssProvider []
      let (textLines :: [Text]) =
            [ "scrollbar {" :: Text
            , "  -GtkRange-slider-width: 200px;"
            , "  -GtkRange-stepper-size: 200px;"
            , "  border-width: 200px;"
            , "  background-color: #ff0000;"
            , "  color: #ff0000;"
            , "  min-width: 50px;"
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
            ]
      let styleData = encodeUtf8 (unlines textLines :: Text)
      #loadFromData cssProvider styleData
      styleContextAddProviderForScreen
        screen
        cssProvider
        (fromIntegral STYLE_PROVIDER_PRIORITY_APPLICATION)

createFontDesc :: IO FontDescription
createFontDesc = do
  fontDesc <- fontDescriptionNew
  fontDescriptionSetFamily fontDesc "DejaVu Sans Mono"
  -- fontDescriptionSetFamily font "Source Code Pro"
  fontDescriptionSetSize fontDesc (16 * SCALE)
  pure fontDesc

setupTermonad :: Application -> ApplicationWindow -> Gtk.Builder -> IO ()
setupTermonad app win builder = do
  void $ Gdk.on win #destroy (#quit app)
  setupScreenStyle
  box <- objFromBuildUnsafe builder "content_box" Box
  fontDesc <- createFontDesc
  note <- new Notebook [#canFocus := False]
  #packStart box note True True 0

  void $ Gdk.on note #pageRemoved $ \_ _ -> do
    pages <- #getNPages note
    when (pages == 0) (#quit app)

  mvarTMState <- newEmptyTMState app win note fontDesc
  terminal <- createTerm handleKeyPress mvarTMState

  void $ onNotebookSwitchPage note $ \_ pageNum -> do
    putStrLn "In callback for onNotebookSwitchPage, about to modify state..."
    maybeRes <- tryTakeMVar mvarTMState
    case maybeRes of
      Nothing -> do
        putStrLn "In callback for onNotebookSwitchPage, mvar was empty. probably not able to proceed."
      Just val -> do
        putStrLn "In callback for onNotebookSwitchPage, mvar was full...?"
        putMVar mvarTMState val
    modifyMVar_ mvarTMState $ \tmState -> do
      putStrLn "In callback for onNotebookSwitchPage, got into the mvar."
      let notebook = tmStateNotebook tmState
          note = tmNotebook notebook
          tabs = tmNotebookTabs notebook
          maybeNewTabs = setFocusFL (fromIntegral pageNum) tabs
      case maybeNewTabs of
        Nothing -> pure tmState
        Just newTabs ->
          pure $
            tmState &
              lensTMStateNotebook . lensTMNotebookTabs .~ newTabs
    putStrLn "In callback for onNotebookSwitchPage, finished modifing state."

  aboutAction <- simpleActionNew "about" Nothing
  void $ onSimpleActionActivate aboutAction (const $ showAboutDialog app)
  actionMapAddAction app aboutAction

  newTabAction <- simpleActionNew "newtab" Nothing
  void $ onSimpleActionActivate newTabAction (\_ -> void $ createTerm handleKeyPress mvarTMState)
  actionMapAddAction app newTabAction
  applicationSetAccelsForAction app "app.newtab" ["<Shift><Ctrl>T"]

  closeTabAction <- simpleActionNew "closetab" Nothing
  void $ onSimpleActionActivate closeTabAction $ \_ -> termExitFocused mvarTMState
  actionMapAddAction app closeTabAction
  applicationSetAccelsForAction app "app.closetab" ["<Shift><Ctrl>W"]

  quitAction <- simpleActionNew "quit" Nothing
  void $ onSimpleActionActivate quitAction $ \_ -> putStrLn "got quit!"
  actionMapAddAction app quitAction
  applicationSetAccelsForAction app "app.quit" ["<Shift><Ctrl>Q"]

  menuBuilder <- builderNewFromString menuText $ fromIntegral (length menuText)
  menuModel <- objFromBuildUnsafe menuBuilder "menubar" MenuModel
  -- applicationSetAppMenu app (Just menuModel)
  #setMenubar app (Just menuModel)

  #showAll win
  focusTerm 0 mvarTMState

appActivate :: Application -> IO ()
appActivate app = do
  putStrLn "called appActivate"
  uiBuilder <-
    builderNewFromString interfaceText $ fromIntegral (length interfaceText)
  builderSetApplication uiBuilder app
  appWin <- objFromBuildUnsafe uiBuilder "appWin" ApplicationWindow
  #addWindow app appWin
  setupTermonad app appWin uiBuilder
  #present appWin

-- | TODO: I should probably be using the actual Gtk.AboutDialog class.
showAboutDialog :: Application -> IO ()
showAboutDialog app = do
  win <- #getActiveWindow app
  builder <- builderNewFromString aboutText $ fromIntegral (length aboutText)
  builderSetApplication builder app
  aboutDialog <- objFromBuildUnsafe builder "aboutDialog" Dialog
  #setTransientFor aboutDialog (Just win)
  #present aboutDialog

appStartup :: Application -> IO ()
appStartup _app = putStrLn "called appStartup"

defaultMain :: IO ()
defaultMain = do
  app <- applicationNew (Just "haskell.termonad") [ApplicationFlagsFlagsNone]
  void $ Gdk.on app #startup (appStartup app)
  void $ Gdk.on app #activate (appActivate app)
  void $ applicationRun app Nothing
