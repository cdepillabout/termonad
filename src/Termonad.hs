{-# LANGUAGE QuasiQuotes #-}
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
