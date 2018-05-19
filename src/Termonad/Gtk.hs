
module Termonad.Gtk where

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
