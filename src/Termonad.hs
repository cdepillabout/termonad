{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Termonad where

import Termonad.Prelude hiding (on)

import GI.Gio
import GI.GLib.Flags (SpawnFlags(..))
import GI.Gtk
import qualified GI.Gtk as Gtk
import GI.Pango
import GI.Vte

defaultMain :: IO ()
defaultMain = do
  void $ Gtk.init Nothing
  win <- new Window [#title := "Hi there"]
  void $ on win #destroy mainQuit

  box <- new Box [#orientation := OrientationVertical]

  button <- new Button [#label := "Click me"]
  void $
    on
      button
      #clicked
      (set button [#sensitive := False, #label := "Thanks for clicking me"])
  #packStart box button False False 0

  font <- fontDescriptionNew
  fontDescriptionSetFamily font "DejaVu Sans Mono"
  fontDescriptionSetSize font (16 * SCALE)

  term <- new Terminal [#fontDesc := font]
  termResVal <-
    #spawnSync
      term
      [PtyFlagsDefault]
      Nothing
      ["/usr/bin/env", "bash"]
      Nothing
      [SpawnFlagsDefault]
      Nothing
      (Nothing :: Maybe Cancellable)
  #packStart box term True True 0

  #add win box
  #showAll win
  Gtk.main
