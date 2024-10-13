
module Termonad.Keys where

import Termonad.Prelude

import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GI.Gdk
  ( EventKey
  , ModifierType(..)
  , getEventKeyHardwareKeycode
  , getEventKeyIsModifier
  , getEventKeyKeyval
  , getEventKeyLength
  , getEventKeyState
  , getEventKeyString
  , getEventKeyType
  )

import Termonad.Types
  ( Key(..)
  , TMState
  , TMState'(tmStateConfig)
  , TMWindowId
  , TMConfig(keys)
  , toKey
  )

showKeys :: EventKey -> IO Bool
showKeys eventKey = do
  eventType <- getEventKeyType eventKey
  maybeString <- getEventKeyString eventKey
  modifiers <- getEventKeyState eventKey
  len <- getEventKeyLength eventKey
  keyval <- getEventKeyKeyval eventKey
  isMod <- getEventKeyIsModifier eventKey
  keycode <- getEventKeyHardwareKeycode eventKey

  putStrLn "key press event:"
  putStrLn $ "  type = " <> show eventType
  putStrLn $ "  str = " <> show maybeString
  putStrLn $ "  mods = " <> show modifiers
  putStrLn $ "  isMod = " <> show isMod
  putStrLn $ "  len = " <> show len
  putStrLn $ "  keyval = " <> show keyval
  putStrLn $ "  keycode = " <> show keycode
  putStrLn ""

  pure True

removeStrangeModifiers :: Key -> Key
removeStrangeModifiers Key{keyVal, keyMods} =
  let reservedModifiers :: Set ModifierType
      reservedModifiers =
        [ ModifierTypeModifierReserved13Mask
        , ModifierTypeModifierReserved14Mask
        , ModifierTypeModifierReserved15Mask
        , ModifierTypeModifierReserved16Mask
        , ModifierTypeModifierReserved17Mask
        , ModifierTypeModifierReserved18Mask
        , ModifierTypeModifierReserved19Mask
        , ModifierTypeModifierReserved20Mask
        , ModifierTypeModifierReserved21Mask
        , ModifierTypeModifierReserved22Mask
        , ModifierTypeModifierReserved23Mask
        , ModifierTypeModifierReserved24Mask
        , ModifierTypeModifierReserved25Mask
        , ModifierTypeModifierReserved29Mask
        ]
  in Key keyVal (Set.difference keyMods reservedModifiers)


handleKeyPress :: TMState -> TMWindowId -> EventKey -> IO Bool
handleKeyPress terState tmWindowId eventKey = do
  -- void $ showKeys eventKey
  keyval <- getEventKeyKeyval eventKey
  modifiers <- getEventKeyState eventKey
  let oldKey = toKey keyval (Set.fromList modifiers)
      newKey = removeStrangeModifiers oldKey
  maybeAction <- Map.lookup newKey . keys . tmStateConfig <$> readMVar terState
  case maybeAction of
    Just action -> action terState tmWindowId
    Nothing -> pure False
