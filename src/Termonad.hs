{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Termonad where

import Termonad.Prelude hiding (on)

import GI.Gtk
import qualified GI.Gtk as Gtk

defaultMain :: IO ()
defaultMain = do
  void $ Gtk.init Nothing
  win <- new Window [#title := "Hi there"]
  void $ on win #destroy mainQuit
  button <- new Button [#label := "Click me"]
  void $
    on
      button
      #clicked
      (set button [#sensitive := False, #label := "Thanks for clicking me"])
  #add win button
  #showAll win
  Gtk.main
