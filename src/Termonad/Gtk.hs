{-# LANGUAGE CPP #-}

-- | This module contains two things:
--
-- 1. Extension functions to libraries like GTK.  These functions wrap up some
--    generic GTK functionality.  They are not Termonad-specific.
--
-- 2. Wrappers around functionality that is only specific to certain versions
--    of libraries like GTK or VTE.
--
--    For instance, 'terminalSetEnableSixelIfExists' is
--    a wrapper around 'terminalSetEnableSixel'.  Sixel support is only availble in
--    vte >= 0.63, so if a user tries to compile Termonad with a version of vte
--    less than 0.63, this function won't do anything.

module Termonad.Gtk where

import Termonad.Prelude

import Data.GI.Base (ManagedPtr, withManagedPtr)
import Data.Text (unpack)
import GHC.Stack (HasCallStack)
import GI.Gdk
  ( GObject
  , castTo
  )
import GI.GdkPixbuf (Pixbuf, pixbufNewFromStream)
import GI.Gio (ApplicationFlags, Cancellable)
import GI.Gio.Objects.MemoryInputStream (memoryInputStreamNewFromData)
import GI.Gtk (Application, IsWidget, Widget(Widget), applicationNew, builderGetObject, toWidget)
import qualified GI.Gtk as Gtk
import GI.Vte
  ( IsTerminal
#ifdef VTE_VERSION_GEQ_0_63
  , terminalSetEnableSixel
#endif
  )
import System.Exit (die)

objFromBuildUnsafe ::
     GObject o => Gtk.Builder -> Text -> (ManagedPtr o -> o) -> IO o
objFromBuildUnsafe builder name constructor = do
  maybePlainObj <- builderGetObject builder name
  case maybePlainObj of
    Nothing -> error $ unpack $ "Couldn't get " <> name <> " from builder!"
    Just plainObj -> do
      maybeNewObj <- castTo constructor plainObj
      case maybeNewObj of
        Nothing -> error $ unpack $ "Got " <> name <> " from builder, but couldn't convert to object!"
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

-- | Wrapper around 'terminalSetEnableSixel'.  The 'terminalSetEnableSixel' function
-- is only available starting with vte-0.63. This function has no effect when
-- compiling against previous versions of vte.
terminalSetEnableSixelIfExists
  :: (HasCallStack, MonadIO m, IsTerminal t)
  => t -- ^ a Terminal
  -> Bool -- ^ whether to enable SIXEL images
  -> m ()
terminalSetEnableSixelIfExists t b = do
#ifdef VTE_VERSION_GEQ_0_63
  terminalSetEnableSixel t b
#endif
  pure ()

-- | Load an image in a 'ByteString' into a 'Pixbuf'.
--
-- Supports all image types that 'pixbufNewFromStream' supports.
imgToPixbuf :: ByteString -> IO Pixbuf
imgToPixbuf imgByteString = do
  inputStream <- memoryInputStreamNewFromData imgByteString Nothing
  maybePixbuf <- pixbufNewFromStream inputStream (Nothing :: Maybe Cancellable)
  case maybePixbuf of
    Nothing ->
      die "imgToPixbuf: Unexpected error when trying to convert an image to a Pixbuf"
    Just pixbuf -> pure pixbuf
