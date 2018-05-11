
module Termonad.Keys where

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

data Key = Key
  { keyVal :: Word32
  , keyMods :: Set ModifierType
  } deriving (Eq, Ord, Show)

toKey :: Word32 -> Set ModifierType -> Key
toKey = Key

keyMap :: Map Key (TerState -> IO Bool)
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
        ]
      altNumKeys =
        imap
          (\i k ->
             (toKey k [ModifierTypeMod1Mask], stopProp (altNumSwitchTerm i))
          )
          numKeys
  in
  mapFromList $
    -- [ ( toKey KEY_T [ModifierTypeControlMask, ModifierTypeShiftMask]
    --   , stopProp createTerm
    --   )
    -- ] <>
    altNumKeys

stopProp :: (TerState -> IO a) -> TerState -> IO Bool
stopProp callback terState = callback terState $> True

handleKeyPress :: TerState -> EventKey -> IO Bool
handleKeyPress terState eventKey = do
  keyval <- get eventKey #keyval
  modifiers <- get eventKey #state
  let key = toKey keyval (setFromList modifiers)
      maybeAction = lookup key keyMap
  case maybeAction of
    Just action -> action terState
    Nothing -> pure False