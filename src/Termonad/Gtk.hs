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
#ifdef darwin_HOST_OS
import GI.Gdk
  ( ScrollDirection(ScrollDirectionSmooth)
  , getEventScrollDeltaX
  , getEventScrollDeltaY
  , getEventScrollDirection
  , setEventScrollDeltaX
  , setEventScrollDeltaY
  )
#endif
import GI.GdkPixbuf (Pixbuf, pixbufNewFromStream)
import GI.Gio (ApplicationFlags, Cancellable)
import GI.Gio.Objects.MemoryInputStream (memoryInputStreamNewFromData)
import GI.Gtk (Application, IsWidget, Widget(Widget), applicationNew, builderGetObject, toWidget)
#ifdef darwin_HOST_OS
import GI.Gtk (onWidgetScrollEvent)
#endif
import qualified GI.Gtk as Gtk
import GI.Vte
  ( IsTerminal
#ifdef darwin_HOST_OS
  , terminalGetCharHeight
  , terminalGetCharWidth
  , terminalGetRowCount
#endif
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

-- | Convert Quartz's surface-pixel scroll deltas into the wheel-notch unit VTE
-- assumes, so that scrolling on macOS moves by the distance the gesture actually
-- asked for.  Does nothing except on macOS.
--
-- __KNOWN BROKEN.  This does not work, and cannot be made to work by adjusting__
-- __the factor.__  It is committed only to record the attempt.  Rewriting the
-- delta has no effect on what VTE goes on to read, because haskell-gi hands a
-- signal callback a /copy/ of a GTK3 event struct, not the event itself:
-- @transientToH@ only borrows the pointer (via
-- @Data.GI.Base.ManagedPtr.withTransient@) when the struct is a registered boxed
-- type, and @GdkEventScroll@ is not one -- there is no
-- @gdk_event_scroll_get_type@ anywhere in GDK, only @GDK_TYPE_EVENT@ for the
-- @GdkEvent@ union.  So the fall-through is @fToH@ to
-- @Data.GI.Base.ManagedPtr.newPtr@, whose own documentation reads \"Wrap a
-- pointer, making a copy of the data\".  Writes land in the copy, read back
-- correctly from the copy, and are invisible to VTE.
--
-- Verified empirically: zeroing the delta outright and returning @False@ still
-- scrolled at full speed, while returning @True@ stopped scrolling completely.
--
-- The analysis of /why/ macOS scrolling is wrong, below, does hold up -- it is
-- only the mechanism that is unworkable.  The replacement takes over VTE's
-- history-scroll path instead of editing events.
--
-- == The underlying model
--
-- There are two kinds of scroll input, and they are measured in different units:
--
--   * Low-precision devices (ordinary wheel mice) report /notches/.  Turning a
--     notch into a distance needs a lines-per-notch factor, conventionally
--     somewhere around 3-5.
--   * High-precision devices (trackpads, and most mice on macOS) report
--     /surface pixels/ -- how far the content should actually move.  These must
--     be used as-is; applying a lines-per-notch factor to them is a category
--     error.
--
-- GTK4 encodes exactly this distinction in the event, as 'GdkScrollUnit'
-- (@GDK_SCROLL_UNIT_WHEEL@ vs @GDK_SCROLL_UNIT_SURFACE@), and its macOS backend
-- tags high-precision deltas @GDK_SCROLL_UNIT_SURFACE@
-- (@gdk\/macos\/gdkmacosdisplay-translate.c@).  kitty arrived at the same model
-- independently: its @wheel_scroll_multiplier@ defaults to @5.0@ and is
-- documented as \"only used for low precision scrolling devices, not for high
-- precision scrolling devices on platforms such as macOS and Wayland\", while
-- @touch_scroll_multiplier@ defaults to @1.0@ -- pixels straight through.
--
-- == What goes wrong here
--
-- GTK3 has no 'GdkScrollUnit'.  Every @GDK_SCROLL_SMOOTH@ delta is implicitly in
-- notch units, and GDK's Quartz backend puts @[NSEvent scrollingDeltaY]@ into
-- that field verbatim (@fill_scroll_event@ in
-- @gdk\/quartz\/gdkevents-quartz.c@ does no scaling at all).  So a pixel
-- quantity arrives labelled as notches.
--
-- VTE then multiplies it by @v = MAX (1., ceil (m_row_count \/ 10.))@ lines per
-- unit (@Terminal::widget_mouse_scroll@ in @vte\/src\/vte.cc@) -- around 5 for a
-- typical window, the same factor kitty uses and for the same reason.  The result
-- is pixel counts inflated by roughly the cell height times that factor.  A light
-- trackpad flick measured about thirty events of one point each: thirty points of
-- finger travel, or about two rows, turned into well over a hundred rows.
--
-- This is a VTE bug rather than a GTK3-only one -- VTE never consults
-- 'GdkScrollUnit' even on GTK4 -- but it is invisible on X11 and Wayland, where
-- smooth deltas really are in notch units.  VTE does carry an internal
-- @m_scroll_unit_is_pixels@ flag, but that governs 'Gtk.Adjustment' units, not
-- the interpretation of incoming events.
--
-- == The correction
--
-- Divide by the cell size /and/ by VTE's lines-per-notch factor.  VTE's own
-- multiplication then cancels, leaving @pixels \/ cellSize@ -- pixel-accurate
-- scrolling, as on every other macOS application, with no tuned constant.
--
-- Rewriting the delta rather than swallowing the event matters: VTE also uses it
-- to synthesise button 4\/5 presses for mouse-aware applications like @vim@ and
-- @less@, and to fake cursor keys on the alternate screen, and those want a
-- corrected figure too.  Fractional remainders are not lost -- the history scroll
-- goes into a @double@ adjustment, and the mouse-tracking path carries the
-- remainder across events explicitly.
--
-- === Known imprecision in mouse-aware applications
--
-- One wrinkle: VTE only applies the lines-per-notch factor on the history-scroll
-- and alternate-screen paths.  When an application has enabled mouse tracking,
-- VTE instead treats the delta directly as a count of button 4\/5 presses, with
-- no such multiplication.  Cancelling the factor is therefore right for the first
-- two paths and over-corrects for the third, by that same factor of around five.
--
-- Concretely: about thirty points of finger travel is roughly two rows, so with
-- applications conventionally scrolling three lines per press the ideal would be
-- something like two thirds of a press; we produce closer to two fifths.  Within
-- a factor of two, and the remainder carries across events rather than being
-- dropped, so it costs a little sensitivity in @vim@ and friends and nothing
-- else.
--
-- This cannot be fixed from out here.  kitty gets it right by branching on
-- whether mouse tracking is active (@mouse.c@, gated on
-- @mouse_tracking_mode == NO_TRACKING@), but VTE makes that choice internally,
-- after we have already handed over the delta, and exposes no way to ask.
--
-- == Related
--
-- Not to be confused with GTK's own Quartz special case for this
-- (@get_scroll_unit@ in @gtk\/gtkscrolledwindow.c@ uses a @scroll_unit@ of 1 on
-- Quartz against @pow(page_size, 2\/3)@ elsewhere) -- that is GTK compensating
-- for the very same thing, but only for widgets that scroll via
-- 'Gtk.ScrolledWindow'.  VTE handles the wheel itself and so gets no such help,
-- which is why this shows up in terminals and not in ordinary GTK applications
-- on macOS.
--
-- Separately, VTE's lines-per-notch factor is considered too coarse even where
-- the units /are/ right:
--
--   * VTE, trackpad scrolls too quickly:
--     <https://gitlab.gnome.org/GNOME/vte/-/issues/2857>
--   * VTE, touch drag scroll fix (the same pixel-vs-row confusion, for
--     touchscreens): <https://gitlab.gnome.org/GNOME/vte/-/issues/283>
--
-- The same family of complaint elsewhere:
--
--   * Ghostty, GTK mouse scrolling too fast:
--     <https://github.com/ghostty-org/ghostty/discussions/9966>
--   * Xpra, window scrolls too fast on macOS -- fixed with a macOS-specific
--     normalisation factor: <https://github.com/Xpra-org/xpra/issues/4309>
--   * GTK, two-finger scrolling is far too fast:
--     <https://gitlab.gnome.org/GNOME/gtk/-/issues/1308>
#ifdef darwin_HOST_OS
terminalNormalizeSmoothScrollDeltas
  :: (MonadIO m, IsTerminal t, IsWidget t) => t -> m ()
terminalNormalizeSmoothScrollDeltas t =
  -- 'scroll-event' is G_SIGNAL_RUN_LAST, so this handler runs before VTE's own
  -- class handler.  Returning False lets VTE go on to handle the event, now
  -- with a delta in the unit it expects.
  --
  -- Mutating the event in place really does affect the event VTE goes on to
  -- see: haskell-gi wraps incoming boxed struct arguments of a signal with
  -- 'Data.GI.Base.ManagedPtr.withTransient', which borrows the pointer rather
  -- than copying it, and it does so specifically so that modifying the incoming
  -- object works (see haskell-gi issue #97).
  void $ onWidgetScrollEvent t $ \eventScroll -> do
    direction <- getEventScrollDirection eventScroll
    -- On Quartz a smooth event always means a high-precision device, and so a
    -- surface-pixel delta.  Discrete up/down events are genuine wheel notches
    -- and want VTE's untouched notch handling.
    when (direction == ScrollDirectionSmooth) $ do
      cellHeight <- terminalGetCharHeight t
      cellWidth <- terminalGetCharWidth t
      rowCount <- terminalGetRowCount t
      let linesPerNotch = vteLinesPerNotch (fromIntegral rowCount)
      when (cellHeight > 0) $ do
        deltaY <- getEventScrollDeltaY eventScroll
        setEventScrollDeltaY eventScroll
          (deltaY / (fromIntegral cellHeight * linesPerNotch))
      when (cellWidth > 0) $ do
        deltaX <- getEventScrollDeltaX eventScroll
        setEventScrollDeltaX eventScroll
          (deltaX / (fromIntegral cellWidth * linesPerNotch))
    pure False
  where
    -- VTE's own lines-per-notch factor, which we have to reproduce in order to
    -- cancel it out.  Mirrors  v = MAX (1., ceil (m_row_count / 10.))  in
    -- Terminal::widget_mouse_scroll (vte/src/vte.cc).
    --
    -- Duplicating an internal VTE constant is not lovely, and it is the one part
    -- of this workaround that could silently drift if VTE changes the factor.
    -- The alternative is worse: without cancelling it, a pixel delta gets
    -- multiplied by a notches-to-lines factor it was never meant to see.
    vteLinesPerNotch :: Double -> Double
    vteLinesPerNotch rowCount =
      max 1 (fromIntegral (ceiling (rowCount / 10) :: Integer))
#else
terminalNormalizeSmoothScrollDeltas
  :: (MonadIO m, IsTerminal t, IsWidget t) => t -> m ()
terminalNormalizeSmoothScrollDeltas _ = pure ()
#endif

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
