
module Termonad.Keys where

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

import Termonad.Types


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

data Key = Key
  { keyVal :: Word32
  , keyMods :: Set ModifierType
  } deriving (Eq, Ord, Show)

toKey :: Word32 -> Set ModifierType -> Key
toKey = Key

keyMap :: Map Key (TMState -> IO Bool)
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
    -- [ ( toKey KEY_T [ModifierTypeControlMask, ModifierTypeShiftMask]
    --   , stopProp createTerm
    --   )
    -- ] <>
    altNumKeys

stopProp :: (TerState -> IO a) -> TerState -> IO Bool
stopProp callback terState = callback terState $> True

handleKeyPress :: TerState -> EventKey -> IO Bool
handleKeyPress terState eventKey = do
  keyval <- get eventKey #keyval
  modifiers <- get eventKey #state
  let key = toKey keyval (setFromList modifiers)
      maybeAction = lookup key keyMap
  case maybeAction of
    Just action -> action terState
    Nothing -> pure False
