
module Termonad.Keys where

import Termonad.Prelude

import Control.Lens (imap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
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
import Termonad.Types (TMState, TMWindowId)


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

data Key = Key
  { keyVal :: !Word32
  , keyMods :: !(Set ModifierType)
  } deriving (Eq, Ord, Show)

toKey :: Word32 -> Set ModifierType -> Key
toKey = Key

keyMap :: Map Key (TMState -> TMWindowId -> IO Bool)
keyMap =
  let numKeys :: [Word32]
      numKeys =
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
      altNumKeys :: [(Key, TMState -> TMWindowId -> IO Bool)]
      altNumKeys =
        imap
          (\i k ->
             (toKey k [ModifierTypeMod1Mask], stopProp (altNumSwitchTerm i))
          )
          numKeys
  in
  Map.fromList altNumKeys

stopProp :: (TMState -> TMWindowId -> IO a) -> TMState -> TMWindowId -> IO Bool
stopProp callback terState tmWinId = callback terState tmWinId $> True

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
      maybeAction = Map.lookup newKey keyMap
  case maybeAction of
    Just action -> action terState tmWindowId
    Nothing -> pure False
