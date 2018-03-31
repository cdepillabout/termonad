{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Termonad where

import Termonad.Prelude

import Control.Lens (imap)
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
import GI.Gio (ApplicationFlags(ApplicationFlagsFlagsNone), applicationRun, noCancellable)
import GI.GLib.Flags (SpawnFlags(..))
import GI.Gtk
  ( Application
  , ApplicationWindow(ApplicationWindow)
  , Box(Box)
  , CssProvider(CssProvider)
  , Notebook(Notebook)
  , Orientation(..)
  , ScrolledWindow(ScrolledWindow)
  , pattern STYLE_PROVIDER_PRIORITY_APPLICATION
  , applicationNew
  , applicationWindowNew
  , builderNewFromString
  , builderSetApplication
  , mainQuit
  , noWidget
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

interfaceDoc :: Document
interfaceDoc =
  [xmlRaw|
    <?xml version="1.0" encoding="UTF-8"?>
    <interface>
      <!-- interface-requires gtk+ 3.8 -->
      <object id="appWin" class="GtkApplicationWindow">
        <property name="title" translatable="yes">Example Application</property>
        <property name="default-width">600</property>
        <property name="default-height">400</property>
        <child>
          <object class="GtkBox" id="content_box">
            <property name="visible">True</property>
            <property name="orientation">vertical</property>
            <child>
              <object class="GtkHeaderBar" id="header">
                <property name="visible">True</property>
                <child type="title">
                  <object class="GtkStackSwitcher" id="tabs">
                    <property name="visible">True</property>
                    <property name="stack">stack</property>
                    <style>
                      <class name="my-special-stackswitcher-class"/>
                      <class name="dark-stackswitcher"/>
                    </style>
                  </object>
                </child>
              </object>
            </child>
            <child>
              <object class="GtkStack" id="stack">
                <property name="visible">True</property>
              </object>
            </child>
          </object>
        </child>
      </object>
    </interface>
   |]

interfaceText :: Text
interfaceText = toStrict $ renderText def interfaceDoc

data Term = Term
  { term :: Terminal
  , unique :: Unique
  }

data Note = Note
  { notebook :: Notebook
  , children :: [Term]
  , font :: FontDescription
  }

type TerState = MVar Note

instance Eq Term where
  (==) :: Term -> Term -> Bool
  (==) = ((==) :: Unique -> Unique -> Bool) `on` (unique :: Term -> Unique)

showKeys :: EventKey -> IO Bool
showKeys eventKey = do
  eventType <- get eventKey #type
  maybeString <- get eventKey #string
  modifiers <- get eventKey #state
  len <- get eventKey #length
  keyval <- get eventKey #keyval
  isMod <- get eventKey #isModifier
  keycode <- get eventKey #hardwareKeycode

  putStrLn "key press event:"
  putStrLn $ "  type = " <> tshow eventType
  putStrLn $ "  str = " <> tshow maybeString
  putStrLn $ "  mods = " <> tshow modifiers
  putStrLn $ "  isMod = " <> tshow isMod
  putStrLn $ "  len = " <> tshow len
  putStrLn $ "  keyval = " <> tshow keyval
  putStrLn $ "  keycode = " <> tshow keycode
  putStrLn ""

  pure True

removeTerm :: [Term] -> Term -> [Term]
removeTerm terms terminal = delete terminal terms

objFromBuildUnsafe ::
     GObject o => Gtk.Builder -> Text -> (ManagedPtr o -> o) -> IO o
objFromBuildUnsafe builder name constructor = do
  maybePlainObj <- #getObject builder name
  case maybePlainObj of
    Nothing -> error $ "Couldn't get " <> unpack name <> " from builder!"
    Just plainObj -> do
      maybeNewObj <- castTo constructor plainObj
      case maybeNewObj of
        Nothing ->
          error $
            "Got " <>
            unpack name <>
            " from builder, but couldn't convert to object!"
        Just obj -> pure obj

data Key = Key
  { keyVal :: Word32
  , keyMods :: Set ModifierType
  } deriving (Eq, Ord, Show)

toKey :: Word32 -> Set ModifierType -> Key
toKey = Key

stopProp :: (TerState -> IO a) -> TerState -> IO Bool
stopProp callback terState = callback terState $> True

keyMap :: Map Key (TerState -> IO Bool)
keyMap =
  let numKeys =
        [ KEY_1
        , KEY_2
        , KEY_3
        , KEY_4
        , KEY_5
        , KEY_6
        , KEY_7
        , KEY_8
        , KEY_9
        ]
      altNumKeys =
        imap
          (\i k ->
             (toKey k [ModifierTypeMod1Mask], stopProp (altNumSwitchTerm i))
          )
          numKeys
  in
  mapFromList $
    [ ( toKey KEY_T [ModifierTypeControlMask, ModifierTypeShiftMask]
      , stopProp createTerm
      )
    ] <>
    altNumKeys

altNumSwitchTerm :: Int -> TerState -> IO ()
altNumSwitchTerm i terState = do
  Note{..} <- readMVar terState
  void $ #setCurrentPage notebook (fromIntegral i)

focusTerm :: Term -> IO ()
focusTerm Term{..} =
  Gdk.set term [#hasFocus := True]

termExit :: ScrolledWindow -> Term -> TerState -> Int32 -> IO ()
termExit scrolledWin terminal terState _exitStatus =
  modifyMVar_ terState $ \Note{..} -> do
    #detachTab notebook scrolledWin
    pure $ Note notebook (removeTerm children terminal) font

createScrolledWin :: IO ScrolledWindow
createScrolledWin = do
  scrolledWin <- new ScrolledWindow []
  #show scrolledWin
  pure scrolledWin

createTerm :: TerState -> IO Term
createTerm terState = do
  scrolledWin <- createScrolledWin
  fontDesc <- withMVar terState (pure . font)
  vteTerm <-
    new Terminal [#fontDesc := fontDesc, #cursorBlinkMode := CursorBlinkModeOn]
  _termResVal <-
    #spawnSync
      vteTerm
      [PtyFlagsDefault]
      Nothing
      ["/usr/bin/env", "bash"]
      Nothing
      [SpawnFlagsDefault]
      Nothing
      noCancellable
  #show vteTerm
  uniq' <- newUnique
  let terminal = Term vteTerm uniq'
  #add scrolledWin (term terminal)
  modifyMVar_ terState $ \Note{..} -> do
    pageIndex <- #appendPage notebook scrolledWin noWidget
    void $ #setCurrentPage notebook pageIndex
    pure $ Note notebook (snoc children terminal) font
  void $ Gdk.on vteTerm #windowTitleChanged $ do
    title <- get vteTerm #windowTitle
    Note{..} <- readMVar terState
    #setTabLabelText notebook scrolledWin title
  void $ Gdk.on (term terminal) #keyPressEvent $ handleKeyPress terState
  void $ Gdk.on scrolledWin #keyPressEvent $ handleKeyPress terState
  void $ Gdk.on (term terminal) #childExited $ termExit scrolledWin terminal terState
  pure terminal

handleKeyPress :: TerState -> EventKey -> IO Bool
handleKeyPress terState eventKey = do
  keyval <- get eventKey #keyval
  modifiers <- get eventKey #state
  let key = toKey keyval (setFromList modifiers)
      maybeAction = lookup key keyMap
  case maybeAction of
    Just action -> action terState
    Nothing -> pure False

indexOf :: forall a. Eq a => a -> [a] -> Maybe Int
indexOf a = go 0
  where
    go :: Int -> [a] -> Maybe Int
    go _ [] = Nothing
    go i (h:ts) = if h == a then Just i else go (i + 1) ts

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

  #showAll win
  focusTerm terminal

appActivate :: Application -> IO ()
appActivate app = do
  builder <-
    builderNewFromString interfaceText $ fromIntegral (length interfaceText)
  builderSetApplication builder app
  appWin <- objFromBuildUnsafe builder "appWin" ApplicationWindow
  #addWindow app appWin
  setupTermonad app appWin builder
  #present appWin

appStartup :: Application -> IO ()
appStartup _app = pure ()
  -- this is probably where I should create actions and builders

defaultMain :: IO ()
defaultMain = do
  app <- applicationNew (Just "haskell.termonad") [ApplicationFlagsFlagsNone]
  void $ Gdk.on app #startup (appStartup app)
  void $ Gdk.on app #activate (appActivate app)
  void $ applicationRun app Nothing
