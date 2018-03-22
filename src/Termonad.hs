{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Termonad where

import Termonad.Prelude hiding (on)

import GI.Gdk
import GI.Gio
import GI.GLib.Flags (SpawnFlags(..))
import GI.Gtk
import qualified GI.Gtk as Gtk
import GI.Pango
import GI.Vte

isShift :: EventKey -> IO Bool
isShift eventKey = do
  modifiers <- get eventKey #state
  pure $ ModifierTypeShiftMask `elem` modifiers

isCtrl :: EventKey -> IO Bool
isCtrl eventKey = do
  modifiers <- get eventKey #state
  pure $ ModifierTypeControlMask `elem` modifiers

isT :: EventKey -> IO Bool
isT eventKey = do
  keyval <- get eventKey #keyval
  pure $ keyval == KEY_T

showKeys :: EventKey -> IO Bool
showKeys eventKey = do
  eventType <- get eventKey #type
  maybeString <- get eventKey #string
  modifiers <- get eventKey #state
  len <- get eventKey #length
  keyval <- get eventKey #keyval
  isMod <- get eventKey #isModifier
  keycode <- get eventKey #hardwareKeycode

  putStrLn "key press event:"
  putStrLn $ "  type = " <> tshow eventType
  putStrLn $ "  str = " <> tshow maybeString
  putStrLn $ "  mods = " <> tshow modifiers
  putStrLn $ "  isMod = " <> tshow isMod
  putStrLn $ "  len = " <> tshow len
  putStrLn $ "  keyval = " <> tshow keyval
  putStrLn $ "  keycode = " <> tshow keycode
  putStrLn ""

  pure True

createTerm :: FontDescription -> Notebook -> IO Terminal
createTerm font notebook = do
  print "creating a new terminal..."
  term <- new Terminal [#fontDesc := font]
  _termResVal <-
    #spawnSync
      term
      [PtyFlagsDefault]
      Nothing
      ["/usr/bin/env", "bash"]
      Nothing
      [SpawnFlagsDefault]
      Nothing
      (Nothing :: Maybe Cancellable)
  print "created termina, appending it..."
  #show term
  #appendPage notebook term noWidget
  print "appended terminal."
  on term #keyPressEvent (openTab font notebook)
  pure term

openTab :: FontDescription -> Notebook -> EventKey -> IO Bool
openTab font notebook eventKey = do
  print "got key press"
  shiftRes <- isShift eventKey
  ctrlRes <- isCtrl eventKey
  tRes <- isT eventKey
  let isShiftCtrlT = shiftRes && ctrlRes && tRes
  putStrLn $ "isShiftCtrlT: " <> tshow isShiftCtrlT
  when isShiftCtrlT $ void $ createTerm font notebook
  putStrLn ""
  pure False

defaultMain :: IO ()
defaultMain = do
  void $ Gtk.init Nothing
  win <- new Gtk.Window [#title := "Hi there"]
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

  notebook <- new Notebook []
  #packStart box notebook True True 0

  void $ createTerm font notebook
  void $ createTerm font notebook

  #add win box
  #showAll win
  Gtk.main
