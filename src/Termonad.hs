{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Termonad where

import Termonad.Prelude

import Data.Unique
import qualified GI.Gdk as Gdk
import GI.Gdk
  ( AttrOp((:=))
  , EventKey
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
  , ModifierType(..)
  , get
  , new
  )
import GI.Gio (noCancellable)
import GI.GLib.Flags (SpawnFlags(..))
import GI.Gtk
  ( Box(Box)
  , Button(Button)
  , Notebook(Notebook)
  , Orientation(..)
  , mainQuit
  , noWidget
  )
import qualified GI.Gtk as Gtk
import GI.Pango
  ( FontDescription
  , pattern SCALE
  , fontDescriptionNew
  , fontDescriptionSetFamily
  , fontDescriptionSetSize
  )
import GI.Vte (PtyFlags(..), Terminal(Terminal))


data Term = Term
  { term :: Terminal
  , unique :: Unique
  }

data Note = Note
  { notebook :: Notebook
  , children :: [Term]
  , font :: FontDescription
  }

type TerState = MVar Note

instance Eq Term where
  (==) :: Term -> Term -> Bool
  (==) = ((==) :: Unique -> Unique -> Bool) `on` (unique :: Term -> Unique)

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

newTerm :: TerState -> IO Term
newTerm terState = do
  fontDesc <- withMVar terState (pure . font)
  terminal <- new Terminal [#fontDesc := fontDesc]
  _termResVal <-
    #spawnSync
      terminal
      [PtyFlagsDefault]
      Nothing
      ["/usr/bin/env", "bash"]
      Nothing
      [SpawnFlagsDefault]
      Nothing
      noCancellable
  #show terminal
  uniq' <- newUnique
  pure $ Term terminal uniq'

removeTerm :: [Term] -> Term -> [Term]
removeTerm terms terminal = delete terminal terms

createTerm :: TerState -> IO ()
createTerm terState = do
  terminal <- newTerm terState
  modifyMVar_ terState $ \Note{..} -> do
    #appendPage notebook (term terminal) noWidget
    pure $ Note notebook (snoc children terminal) font
  Gdk.on (term terminal) #keyPressEvent (handleKeyPress terState)
  Gdk.on (term terminal) #childExited $ \_ -> do
    modifyMVar_ terState $ \Note{..} -> do
      #detachTab notebook (term terminal)
      pure $ Note notebook (removeTerm children terminal) font
  pure ()

handleKeyPress :: TerState -> EventKey -> IO Bool
handleKeyPress terState eventKey = do
  keyval <- get eventKey #keyval
  modifiers <- get eventKey #state

  when (keyval == KEY_T && ModifierTypeControlMask `elem` modifiers) $
    createTerm terState

  let numKeys =
        [ KEY_1
        , KEY_2
        , KEY_3
        , KEY_4
        , KEY_5
        , KEY_6
        , KEY_7
        , KEY_8
        , KEY_9
        ]

  when ((keyval `elem` numKeys) && (ModifierTypeMod1Mask `elem` modifiers)) $
    putStrLn "hello"

  pure False

defaultMain :: IO ()
defaultMain = do
  void $ Gtk.init Nothing
  win <- new Gtk.Window [#title := "Hi there"]
  void $ Gdk.on win #destroy mainQuit

  box <- new Box [#orientation := OrientationVertical]

  button <- new Button [#label := "Click me"]
  void $
    Gdk.on
      button
      #clicked
      (Gdk.set button [#sensitive := False, #label := "Thanks for clicking me"])
  #packStart box button False False 0

  fontDesc <- fontDescriptionNew
  fontDescriptionSetFamily fontDesc "DejaVu Sans Mono"
  -- fontDescriptionSetFamily font "Source Code Pro"
  fontDescriptionSetSize fontDesc (16 * SCALE)

  note <- new Notebook []
  #packStart box note True True 0

  terState <-
    newMVar $
      Note
        { notebook = note
        , children = []
        , font = fontDesc
        }

  createTerm terState
  createTerm terState

  #add win box
  #showAll win
  Gtk.main
