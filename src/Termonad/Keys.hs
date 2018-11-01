
module Termonad.Keys where

import Termonad.Prelude

import Control.Lens (imap)
import GI.Gdk
  ( EventKey
  , pattern KEY_0
  , pattern KEY_1
  , pattern KEY_2
  , pattern KEY_3
  , pattern KEY_4
  , pattern KEY_5
  , pattern KEY_6
  , pattern KEY_7
  , pattern KEY_8
  , pattern KEY_9
  , ModifierType(..)
  , getEventKeyHardwareKeycode
  , getEventKeyIsModifier
  , getEventKeyKeyval
  , getEventKeyLength
  , getEventKeyState
  , getEventKeyString
  , getEventKeyType
  )

import Termonad.Term (altNumSwitchTerm)
import Termonad.Types (TMState)


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
  putStrLn $ "  type = " <> tshow eventType
  putStrLn $ "  str = " <> tshow maybeString
  putStrLn $ "  mods = " <> tshow modifiers
  putStrLn $ "  isMod = " <> tshow isMod
  putStrLn $ "  len = " <> tshow len
  putStrLn $ "  keyval = " <> tshow keyval
  putStrLn $ "  keycode = " <> tshow keycode
  putStrLn ""

  pure True

data Key = Key
  { keyVal :: Word32
  , keyMods :: Set ModifierType
  } deriving (Eq, Ord, Show)

toKey :: Word32 -> Set ModifierType -> Key
toKey = Key

keyMap :: Map Key (TMState -> IO Bool)
keyMap =
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
        , KEY_0
        ]
      altNumKeys =
        imap
          (\i k ->
             (toKey k [ModifierTypeMod1Mask], stopProp (altNumSwitchTerm i))
          )
          numKeys
  in
  mapFromList altNumKeys

stopProp :: (TMState -> IO a) -> TMState -> IO Bool
stopProp callback terState = callback terState $> True

removeStrangeModifiers :: Key -> Key
removeStrangeModifiers Key{keyVal, keyMods} =
  let reservedModifiers =
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
  in Key keyVal (difference keyMods reservedModifiers)


handleKeyPress :: TMState -> EventKey -> IO Bool
handleKeyPress terState eventKey = do
  -- void $ showKeys eventKey
  keyval <- getEventKeyKeyval eventKey
  modifiers <- getEventKeyState eventKey
  let oldKey = toKey keyval (setFromList modifiers)
      newKey = removeStrangeModifiers oldKey
      maybeAction = lookup newKey keyMap
  case maybeAction of
    Just action -> action terState
    Nothing -> pure False
