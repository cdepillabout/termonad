{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Termonad where

import Termonad.Prelude hiding (on)

import GI.Gio
import GI.GLib.Flags (SpawnFlags(..))
import GI.Gtk
import qualified GI.Gtk as Gtk
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
  boxPackStart box button False False 0

  term <- new Terminal []
  termResVal <-
    terminalSpawnSync
      term
      [PtyFlagsDefault]
      Nothing
      ["/usr/bin/env", "bash"]
      Nothing
      [SpawnFlagsDefault]
      Nothing
      (Nothing :: Maybe Cancellable)
  boxPackStart box term True True 0

  #add win box
  #showAll win
  Gtk.main
