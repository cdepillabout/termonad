{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Termonad where

import Termonad.Prelude hiding (on)

import GI.Gtk
import qualified GI.Gtk as Gtk
-- import Data.GI.Base (AttrOp((:=)), new, on, set)
-- import Data.GI.Base

defaultMain :: IO ()
defaultMain = do
  Gtk.init Nothing
  win <- new Gtk.Window [ #title := "Hi there" ]
  on win #destroy Gtk.mainQuit
  button <- new Gtk.Button [ #label := "Click me" ]
  on
    button
    #clicked
      ( set
          button
          [ #sensitive := False
          , #label := "Thanks for clicking me"
          ]
      )
  #add win button
  #showAll win
  Gtk.main
