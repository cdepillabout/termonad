{-# LANGUAGE CPP #-}

module Termonad.Gtk where

import Termonad.Prelude

import Control.Monad.Fail (MonadFail, fail)
import Data.GI.Base (ManagedPtr, withManagedPtr)
import Data.GI.Base.GObject (gtypeFromInstance)
import GHC.Stack (HasCallStack)
import GI.Gdk
  ( GObject
  , castTo
  )
import GI.Gio (ApplicationFlags)
import GI.Gtk (Application, IsWidget, Widget(Widget), applicationNew, builderGetObject, toWidget)
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

-- | Unsafely creates a new 'Application'.  This calls 'fail' if it cannot
-- create the 'Application' for some reason.
--
-- This can fail for different reasons, one of which being that application
-- name does not have a period in it.
appNew ::
     (HasCallStack, MonadIO m, MonadFail m)
  => Maybe Text
  -- ^ The application name.  Must have a period in it if specified.  If passed
  -- as 'Nothing', then no application name will be used.
  -> [ApplicationFlags]
  -> m Application
appNew appName appFlags = do
  maybeApp <- applicationNew appName appFlags
  case maybeApp of
    Nothing -> fail "Could not create application for some reason!"
    Just app -> pure app

-- | Tests to see if two GTK widgets point to the same thing.  This should only
-- happen if they are actually the same thing.
widgetEq :: (MonadIO m, IsWidget a, IsWidget b) => a -> b -> m Bool
widgetEq a b = do
  Widget managedPtrA <- toWidget a
  Widget managedPtrB <- toWidget b
  liftIO $
    withManagedPtr managedPtrA $ \ptrA ->
      withManagedPtr managedPtrB $ \ptrB ->
        pure (ptrA == ptrB)

printWidgetTree :: Gtk.IsWidget a => a -> IO ()
printWidgetTree widget_ = do
  widget <- Gtk.toWidget widget_
  go "" widget
  where
    go :: Text -> Gtk.Widget -> IO ()
    go indent w = do
      type_ <- gtypeFromInstance w
      name <- Gtk.gtypeName type_
      let ptr = Gtk.managedForeignPtr . Gtk.toManagedPtr $ w
      putStrLn $ indent <> pack name <> "  " <> pack (show ptr)
      maybeContainer <- Gtk.castTo Gtk.Container w
      for_ maybeContainer $ \container -> do
        children <- Gtk.containerGetChildren container
        for_ children $ \child -> do
          go ("  " <> indent) child
