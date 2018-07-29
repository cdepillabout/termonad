{-# LANGUAGE CPP #-}

module Termonad.Gtk where

import Termonad.Prelude

import GHC.Stack (HasCallStack)
import GI.Gdk
  ( GObject
  , ManagedPtr
  , castTo
  )
import GI.Gio (ApplicationFlags)
import GI.Gtk (Application, applicationNew, builderGetObject)
import qualified GI.Gtk as Gtk


objFromBuildUnsafe ::
     GObject o => Gtk.Builder -> Text -> (ManagedPtr o -> o) -> IO o
objFromBuildUnsafe builder name constructor = do
  maybePlainObj <- builderGetObject builder name
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

appNew :: (HasCallStack, MonadIO m) => Maybe Text -> [ApplicationFlags] -> m Application
appNew appName appFlags = do
  maybeApp <- applicationNew appName appFlags
  case maybeApp of
    Nothing -> fail "Could not create application for some reason!"
    Just app -> pure app
