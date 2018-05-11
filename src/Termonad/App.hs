
module Termonad.App where

setupTermonad :: Application -> ApplicationWindow -> Gtk.Builder -> IO ()
setupTermonad app win builder = do
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

  void $ Gdk.on win #destroy (#quit app)


  box <- objFromBuildUnsafe builder "content_box" Box

  fontDesc <- fontDescriptionNew
  fontDescriptionSetFamily fontDesc "DejaVu Sans Mono"
  -- fontDescriptionSetFamily font "Source Code Pro"
  fontDescriptionSetSize fontDesc (16 * SCALE)

  note <- new Notebook [#canFocus := False]
  #packStart box note True True 0

  terState <-
    newMVar $
      Note
        { notebook = note
        , children = []
        , font = fontDesc
        }

  void $ Gdk.on note #pageRemoved $ \_ _ -> do
    pages <- #getNPages note
    when (pages == 0) (#quit app)

  terminal <- createTerm terState

  aboutAction <- simpleActionNew "about" Nothing
  void $ onSimpleActionActivate aboutAction (const $ showAboutDialog app)
  applicationSddAction app aboutAction

  newTabAction <- simpleActionNew "newtab" Nothing
  void $ onSimpleActionActivate newTabAction (\_ -> void $ createTerm terState)
  actionMapAddAction app newTabAction
  applicationSetAccelsForAction app "app.newtab" ["<Shift><Ctrl>T"]

  closeTabAction <- simpleActionNew "closetab" Nothing
  void $ onSimpleActionActivate closeTabAction (\_ -> putStrLn "got close tab!")
  actionMapAddAction app closeTabAction
  applicationSetAccelsForAction app "app.closetab" ["<Shift><Ctrl>W"]

  quitAction <- simpleActionNew "quit" Nothing
  void $ onSimpleActionActivate quitAction (\_ -> putStrLn "got quit!")
  actionMapAddAction app quitAction
  applicationSetAccelsForAction app "app.quit" ["<Shift><Ctrl>Q"]

  menuBuilder <- builderNewFromString menuText $ fromIntegral (length menuText)
  menuModel <- objFromBuildUnsafe menuBuilder "menubar" MenuModel
  -- applicationSetAppMenu app (Just menuModel)
  #setMenubar app (Just menuModel)

  #showAll win
  focusTerm terminal

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
