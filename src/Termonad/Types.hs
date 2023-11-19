{-# OPTIONS_GHC -fno-warn-orphans #-}

module Termonad.Types where

import Termonad.Prelude

import Control.Lens (ifoldMap)
import Data.FocusList (FocusList, emptyFL, getFocusItemFL, lengthFL)
import Data.Foldable (toList)
import Data.Unique (Unique, hashUnique, newUnique)
import Data.Yaml
  ( FromJSON(parseJSON)
  , ToJSON(toJSON)
  , Value(String)
  , withText
  )
import GI.Gtk
  ( Application
  , ApplicationWindow
  , IsWidget
  , Label
  , Notebook
  , ScrolledWindow
  , Widget
  , notebookGetCurrentPage
  , notebookGetNthPage
  , notebookGetNPages
  )
import GI.Pango (FontDescription, fontDescriptionGetSize, fontDescriptionGetSizeIsAbsolute, pattern SCALE, fontDescriptionGetFamily, fontDescriptionNew, fontDescriptionSetFamily, fontDescriptionSetSize, fontDescriptionSetAbsoluteSize)
import GI.Vte (Terminal, CursorBlinkMode(..))
import Termonad.Gtk (widgetEq)
import Termonad.IdMap (IdMap, IdMapKey, singletonIdMap, lookupIdMap)
import Text.Pretty.Simple (pPrint)

-- | A wrapper around a VTE 'Terminal'.  This also stores the process ID of the
-- process running on this terminal, as well as a 'Unique' that can be used for
-- comparing terminals.
data TMTerm = TMTerm
  { term :: !Terminal
    -- ^ The actual 'Terminal'.
  , pid :: !Int
    -- ^ The process ID of the process running in 'term'.
  , unique :: !Unique
    -- ^ A 'Unique' for comparing different 'TMTerm' for uniqueness.
  }

instance Show TMTerm where
  showsPrec :: Int -> TMTerm -> ShowS
  showsPrec d TMTerm{..} =
    showParen (d > 10) $
      showString "TMTerm {" .
      showString "term = " .
      showString "(GI.GTK.Terminal)" .
      showString ", " .
      showString "pid = " .
      showsPrec (d + 1) pid .
      showString ", " .
      showString "unique = " .
      showsPrec (d + 1) (hashUnique unique) .
      showString "}"

-- | A container that holds everything in a given terminal window.  The 'term'
-- in the 'TMTerm' is inside the 'tmNotebookTabTermContainer' 'ScrolledWindow'.
-- The notebook tab 'Label' is also available.
data TMNotebookTab = TMNotebookTab
  { tmNotebookTabTermContainer :: !ScrolledWindow
    -- ^ The 'ScrolledWindow' holding the VTE 'Terminal'.
  , tmNotebookTabTerm :: !TMTerm
    -- ^ The 'Terminal' insidie the 'ScrolledWindow'.
  , tmNotebookTabLabel :: !Label
    -- ^ The 'Label' holding the title of the 'Terminal' in the 'Notebook' tab.
  }

instance Show TMNotebookTab where
  showsPrec :: Int -> TMNotebookTab -> ShowS
  showsPrec d TMNotebookTab{..} =
    showParen (d > 10) $
      showString "TMNotebookTab {" .
      showString "tmNotebookTabTermContainer = " .
      showString "(GI.GTK.ScrolledWindow)" .
      showString ", " .
      showString "tmNotebookTabTerm = " .
      showsPrec (d + 1) tmNotebookTabTerm .
      showString ", " .
      showString "tmNotebookTabLabel = " .
      showString "(GI.GTK.Label)" .
      showString "}"

-- | This holds the GTK 'Notebook' containing multiple tabs of 'Terminal's.  We
-- keep a separate list of terminals in 'tmNotebookTabs'.
data TMNotebook = TMNotebook
  { tmNotebook :: !Notebook
    -- ^ This is the GTK 'Notebook' that holds multiple tabs of 'Terminal's.
  , tmNotebookTabs :: !(FocusList TMNotebookTab)
    -- ^ A 'FocusList' containing references to each individual 'TMNotebookTab'.
  }

instance Show TMNotebook where
  showsPrec :: Int -> TMNotebook -> ShowS
  showsPrec d TMNotebook{..} =
    showParen (d > 10) $
      showString "TMNotebook {" .
      showString "tmNotebook = " .
      showString "(GI.GTK.Notebook)" .
      showString ", " .
      showString "tmNotebookTabs = " .
      showsPrec (d + 1) tmNotebookTabs .
      showString "}"

getNotebookFromTMState :: TMState -> TMWindowId -> IO Notebook
getNotebookFromTMState mvarTMState tmWinId = do
  tmNote <- getTMNotebookFromTMState mvarTMState tmWinId
  pure $ tmNotebook tmNote

getNotebookFromTMState' :: TMState' -> TMWindowId -> IO Notebook
getNotebookFromTMState' tmState tmWinId = do
  tmNote <- getTMNotebookFromTMState' tmState tmWinId
  pure $ tmNotebook tmNote

getTMNotebookFromTMState :: TMState -> TMWindowId -> IO TMNotebook
getTMNotebookFromTMState mvarTMState tmWinId = do
  tmWin <- getTMWindowFromTMState mvarTMState tmWinId
  pure $ tmWindowNotebook tmWin

getTMNotebookFromTMState' :: TMState' -> TMWindowId -> IO TMNotebook
getTMNotebookFromTMState' tmState tmWinId = do
  tmWin <- getTMWindowFromTMState' tmState tmWinId
  pure $ tmWindowNotebook tmWin

data TMWindow = TMWindow
  { tmWindowAppWin :: !ApplicationWindow
  , tmWindowNotebook :: !TMNotebook
  }

instance Show TMWindow where
  showsPrec :: Int -> TMWindow -> ShowS
  showsPrec d TMWindow{..} =
    showParen (d > 10) $
      showString "TMWindow {" .
      showString "tmWindowAppWin = " .
      showString "(GI.GTK.ApplicationWindow)" .
      showString ", " .
      showString "tmWindowNotebook = " .
      showsPrec (d + 1) tmWindowNotebook .
      showString "}"

type TMWindowId = IdMapKey

-- | Get a given 'TMWindow' from a set of 'TMWindow's given a 'TMWindowId'.
--
-- This throws an error if the 'TMWindowId' can't be found within the 'IdMap'.
getTMWindowFromWins :: IdMap TMWindow -> TMWindowId -> IO TMWindow
getTMWindowFromWins tmWins tmWinId =
  case lookupIdMap tmWinId tmWins of
    Nothing -> error $ "getTMWindowFromWins: trying to get id " <> show tmWinId <> " from wins, but doesn't exist: " <> show tmWins
    Just tmWin -> pure tmWin

-- | Get a given 'TMWindow' froma a 'TMState' given a 'TMWindowId'.
--
-- This throws an error if the 'TMWindowId' can't be found within the 'TMState'.
getTMWindowFromTMState :: TMState -> TMWindowId -> IO TMWindow
getTMWindowFromTMState mvarTMState tmWinId = do
  TMState{tmStateWindows} <- readMVar mvarTMState
  getTMWindowFromWins tmStateWindows tmWinId

getTMWindowFromTMState' :: TMState' -> TMWindowId -> IO TMWindow
getTMWindowFromTMState' tmState tmWinId =
  getTMWindowFromWins (tmStateWindows tmState) tmWinId

data TMState' = TMState
  { tmStateApp :: !Application
  , tmStateConfig :: !TMConfig
  , tmStateFontDesc :: !FontDescription
  , tmStateWindows :: !(IdMap TMWindow)
  }

instance Show TMState' where
  showsPrec :: Int -> TMState' -> ShowS
  showsPrec d TMState{..} =
    showParen (d > 10) $
      showString "TMState {" .
      showString "tmStateApp = " .
      showString "(GI.GTK.Application)" .
      showString ", " .
      showString "tmStateConfig = " .
      showsPrec (d + 1) tmStateConfig .
      showString ", " .
      showString "tmStateFontDesc = " .
      showString "(GI.Pango.FontDescription)" .
      showString ", " .
      showString "tmStateWindows = " .
      showsPrec (d + 1) tmStateWindows .
      showString "}"

type TMState = MVar TMState'

instance Eq TMTerm where
  (==) :: TMTerm -> TMTerm -> Bool
  (==) = (==) `on` (unique :: TMTerm -> Unique)

instance Eq TMNotebookTab where
  (==) :: TMNotebookTab -> TMNotebookTab -> Bool
  (==) = (==) `on` tmNotebookTabTerm

createTMTerm :: Terminal -> Int -> Unique -> TMTerm
createTMTerm trm pd unq =
  TMTerm
    { term = trm
    , pid = pd
    , unique = unq
    }

newTMTerm :: Terminal -> Int -> IO TMTerm
newTMTerm trm pd = createTMTerm trm pd <$> newUnique

getFocusedTermFromState :: TMState -> TMWindowId -> IO (Maybe Terminal)
getFocusedTermFromState mvarTMState tmWinId =
  withMVar mvarTMState go
  where
    go :: TMState' -> IO (Maybe Terminal)
    go tmState = do
      tmNote <- getTMNotebookFromTMState' tmState tmWinId
      let maybeNotebookTab = getFocusItemFL $ tmNotebookTabs tmNote
      pure $ fmap (term . tmNotebookTabTerm) maybeNotebookTab

createTMNotebookTab :: Label -> ScrolledWindow -> TMTerm -> TMNotebookTab
createTMNotebookTab tabLabel scrollWin trm =
  TMNotebookTab
    { tmNotebookTabTermContainer = scrollWin
    , tmNotebookTabTerm = trm
    , tmNotebookTabLabel = tabLabel
    }

createTMNotebook :: Notebook -> FocusList TMNotebookTab -> TMNotebook
createTMNotebook note tabs =
  TMNotebook
    { tmNotebook = note
    , tmNotebookTabs = tabs
    }

createEmptyTMNotebook :: Notebook -> TMNotebook
createEmptyTMNotebook notebook = createTMNotebook notebook emptyFL

notebookToList :: Notebook -> IO [Widget]
notebookToList notebook =
  unfoldHelper 0 []
  where unfoldHelper :: Int32 -> [Widget] -> IO [Widget]
        unfoldHelper index32 acc = do
          notePage <- notebookGetNthPage notebook index32
          case notePage of
            Nothing -> pure acc
            Just notePage' -> unfoldHelper (index32 + 1) (acc ++ [notePage'])

createTMWindow :: ApplicationWindow -> TMNotebook -> TMWindow
createTMWindow appwin notebook =
  TMWindow
    { tmWindowAppWin = appwin
    , tmWindowNotebook = notebook
    }

newEmptyTMState :: TMConfig -> Application -> ApplicationWindow -> Notebook -> FontDescription -> IO (TMState, TMWindowId)
newEmptyTMState tmConfig app appWin note fontDesc = do
  let tmnote = createEmptyTMNotebook note
      tmwin = createTMWindow appWin tmnote
      (tmwinId, tmwins) = singletonIdMap tmwin
  tmState <-
    newMVar $
      TMState
        { tmStateApp = app
        , tmStateConfig = tmConfig
        , tmStateFontDesc = fontDesc
        , tmStateWindows = tmwins
        }
  pure (tmState, tmwinId)

------------
-- Config --
------------

-- | The font size for the Termonad terminal.  There are two ways to set the
-- fontsize, corresponding to the two different ways to set the font size in
-- the Pango font rendering library.
--
-- If you're not sure which to use, try 'FontSizePoints' first and see how it
-- looks.  It should generally correspond to font sizes you are used to from
-- other applications.
data FontSize
  = FontSizePoints Int
    -- ^ This sets the font size based on \"points\".  The conversion between a
    -- point and an actual size depends on the system configuration and the
    -- output device.  The function 'GI.Pango.fontDescriptionSetSize' is used
    -- to set the font size.  See the documentation for that function for more
    -- info.
  | FontSizeUnits Double
    -- ^ This sets the font size based on \"device units\".  In general, this
    -- can be thought of as one pixel.  The function
    -- 'GI.Pango.fontDescriptionSetAbsoluteSize' is used to set the font size.
    -- See the documentation for that function for more info.
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

-- | The default 'FontSize' used if not specified.
--
-- >>> defaultFontSize
-- FontSizePoints 12
defaultFontSize :: FontSize
defaultFontSize = FontSizePoints 12

-- | Modify a 'FontSize' by adding some value.
--
-- >>> modFontSize 1 (FontSizePoints 13)
-- FontSizePoints 14
-- >>> modFontSize 1 (FontSizeUnits 9.0)
-- FontSizeUnits 10.0
--
-- You can reduce the font size by passing a negative value.
--
-- >>> modFontSize (-2) (FontSizePoints 13)
-- FontSizePoints 11
--
-- If you try to create a font size less than 1, then the old font size will be
-- used.
--
-- >>> modFontSize (-10) (FontSizePoints 5)
-- FontSizePoints 5
-- >>> modFontSize (-1) (FontSizeUnits 1.0)
-- FontSizeUnits 1.0
modFontSize :: Int -> FontSize -> FontSize
modFontSize i (FontSizePoints oldPoints) =
  let newPoints = oldPoints + i
  in FontSizePoints $ if newPoints < 1 then oldPoints else newPoints
modFontSize i (FontSizeUnits oldUnits) =
  let newUnits = oldUnits + fromIntegral i
  in FontSizeUnits $ if newUnits < 1 then oldUnits else newUnits

fontSizeFromFontDescription :: FontDescription -> IO FontSize
fontSizeFromFontDescription fontDesc = do
  currSize <- fontDescriptionGetSize fontDesc
  currAbsolute <- fontDescriptionGetSizeIsAbsolute fontDesc
  pure $
    if currAbsolute
    then FontSizeUnits $ fromIntegral currSize / fromIntegral SCALE
    else
      let fontRatio :: Double = fromIntegral currSize / fromIntegral SCALE
      in FontSizePoints $ round fontRatio

-- | Create a 'FontDescription' from a 'FontSize' and font family.
createFontDesc
  :: FontSize
  -> Text
  -- ^ font family
  -> IO FontDescription
createFontDesc fontSz fontFam = do
  fontDesc <- fontDescriptionNew
  fontDescriptionSetFamily fontDesc fontFam
  setFontDescSize fontDesc fontSz
  pure fontDesc

-- | Set the size of a 'FontDescription' from a 'FontSize'.
setFontDescSize :: FontDescription -> FontSize -> IO ()
setFontDescSize fontDesc (FontSizePoints points) =
  fontDescriptionSetSize fontDesc $ fromIntegral (points * fromIntegral SCALE)
setFontDescSize fontDesc (FontSizeUnits units) =
  fontDescriptionSetAbsoluteSize fontDesc $ units * fromIntegral SCALE

-- | Create a 'FontDescription' from the 'fontSize' and 'fontFamily' inside a 'TMConfig'.
createFontDescFromConfig :: TMConfig -> IO FontDescription
createFontDescFromConfig tmConfig = do
  let fontConf = fontConfig (options tmConfig)
  createFontDesc (fontSize fontConf) (fontFamily fontConf)

-- | Settings for the font to be used in Termonad.
data FontConfig = FontConfig
  { fontFamily :: !Text
    -- ^ The font family to use.  Example: @"DejaVu Sans Mono"@ or @"Source Code Pro"@
  , fontSize :: !FontSize
    -- ^ The font size.
  } deriving (Eq, FromJSON, Generic, Show, ToJSON)

-- | The default 'FontConfig' to use if not specified.
--
-- >>> defaultFontConfig == FontConfig {fontFamily = "Monospace", fontSize = defaultFontSize}
-- True
defaultFontConfig :: FontConfig
defaultFontConfig =
  FontConfig
    { fontFamily = "Monospace"
    , fontSize = defaultFontSize
    }

fontConfigFromFontDescription :: FontDescription -> IO (Maybe FontConfig)
fontConfigFromFontDescription fontDescription = do
  fontSize <- fontSizeFromFontDescription fontDescription
  maybeFontFamily <- fontDescriptionGetFamily fontDescription
  return $ (`FontConfig` fontSize) <$> maybeFontFamily

-- | This data type represents an option that can either be 'Set' or 'Unset'.
--
-- This data type is used in situations where leaving an option unset results
-- in a special state that is not representable by setting any specific value.
--
-- Examples of this include the 'cursorFgColour' and 'cursorBgColour' options
-- supplied by the 'ColourConfig' @ConfigExtension@.  By default,
-- 'cursorFgColour' and 'cursorBgColour' are both 'Unset'.  However, when
-- 'cursorBgColour' is 'Set', 'cursorFgColour' defaults to the color of the text
-- underneath.  There is no way to represent this by explicitly setting
-- 'cursorFgColour'.
data Option a = Unset | Set !a
  deriving (Show, Read, Eq, Ord, Functor, Foldable)

-- | Run a function over the value contained in an 'Option'. Return 'mempty'
-- when 'Option' is 'Unset'.
--
-- >>> whenSet (Set [1,2,3]) (++ [4,5,6]) :: [Int]
-- [1,2,3,4,5,6]
-- >>> whenSet Unset (++ [4,5,6]) :: [Int]
-- []
whenSet :: Monoid m => Option a -> (a -> m) -> m
whenSet = \case
  Unset -> const mempty
  Set x -> \f -> f x

-- | Whether or not to show the scroll bar in a terminal.
data ShowScrollbar
  = ShowScrollbarNever -- ^ Never show the scroll bar, even if there are too
                       -- many lines on the terminal to show all at once.  You
                       -- should still be able to scroll with the mouse wheel.
  | ShowScrollbarAlways -- ^ Always show the scrollbar, even if it is not
                        -- needed.
  | ShowScrollbarIfNeeded -- ^ Only show the scrollbar if there are too many
                          -- lines on the terminal to show all at once.
  deriving (Enum, Eq, Generic, FromJSON, Show, ToJSON)

showScrollbarToString :: ShowScrollbar -> Text
showScrollbarToString = \case
  ShowScrollbarNever -> "never"
  ShowScrollbarAlways -> "always"
  ShowScrollbarIfNeeded -> "if-needed"

showScrollbarFromString :: Text -> Maybe ShowScrollbar
showScrollbarFromString = \case
  "never" -> Just ShowScrollbarNever
  "always" -> Just ShowScrollbarAlways
  "if-needed" -> Just ShowScrollbarIfNeeded
  _ -> Nothing

-- | Whether or not to show the tab bar for switching tabs.
data ShowTabBar
  = ShowTabBarNever -- ^ Never show the tab bar, even if there are multiple tabs
                    -- open.  This may be confusing if you plan on using multiple tabs.
  | ShowTabBarAlways -- ^ Always show the tab bar, even if you only have one tab open.
  | ShowTabBarIfNeeded  -- ^ Only show the tab bar if you have multiple tabs open.
  deriving (Enum, Eq, Generic, FromJSON, Show, ToJSON)

showTabBarToString :: ShowTabBar -> Text
showTabBarToString = \case
  ShowTabBarNever -> "never"
  ShowTabBarAlways -> "always"
  ShowTabBarIfNeeded -> "if-needed"

showTabBarFromString :: Text -> Maybe ShowTabBar
showTabBarFromString = \case
  "never" -> Just ShowTabBarNever
  "always" -> Just ShowTabBarAlways
  "if-needed" -> Just ShowTabBarIfNeeded
  _ -> Nothing

-- | Configuration options for Termonad.
--
-- See 'defaultConfigOptions' for the default values.
data ConfigOptions = ConfigOptions
  { fontConfig :: !FontConfig
    -- ^ Specific options for fonts.
  , showScrollbar :: !ShowScrollbar
    -- ^ When to show the scroll bar.
  , scrollbackLen :: !Integer
    -- ^ The number of lines to keep in the scroll back history for each terminal.
  , confirmExit :: !Bool
    -- ^ Whether or not to ask you for confirmation when closing individual
    -- terminals or Termonad itself.  It is generally safer to keep this as
    -- 'True'.
  , wordCharExceptions :: !Text
    -- ^ When double-clicking on text in the terminal with the mouse, Termonad
    -- will use this value to determine what to highlight.  The individual
    -- characters in this list will be counted as part of a word.
    --
    -- For instance if 'wordCharExceptions' is @""@, then when you double-click
    -- on the text @http://@, only the @http@ portion will be highlighted.  If
    -- 'wordCharExceptions' is @":"@, then the @http:@ portion will be
    -- highlighted.
  , showMenu :: !Bool
    -- ^ Whether or not to show the @File@ @Edit@ etc menu.
  , showTabBar :: !ShowTabBar
    -- ^ When to show the tab bar.
  , cursorBlinkMode :: !CursorBlinkMode
    -- ^ How to handle cursor blink.
  , boldIsBright :: !Bool
    -- ^ This option controls whether or not to force bold text to use colors
    -- from the 'Termonad.Config.Colour.ExtendedPalatte'.
    --
    -- If 'True', then colored bold text will /always/ use colors from the
    -- 'Termonad.Config.Colour.ExtendedPalatte'.  There will be no way to print
    -- bold text colored with the 'Termonad.Config.Colour.BasicPalatte'.
    --
    -- This often isn't a big problem, since many TUI applications use
    -- bold in combination with colors from the 'Termonad.Config.Colour.ExtendedPalatte'.
    -- Also, the VTE default blue color can be difficult to read with a dark
    -- background, and enabling this can work around the problem.
    -- See <https://github.com/cdepillabout/termonad/issues/177> for more information.
    --
    -- If 'False', then bold can be applied separately to colors from both the
    -- 'Termonad.Config.Colour.BasicPalatte' and
    -- 'Termonad.Config.Colour.ExtendedPalatte'.
  , enableSixel :: !Bool
    -- ^ Enable SIXEL to draw graphics in a terminal.
    --
    -- In order for this option to do anything, you need to be using a version
    -- of VTE >= 0.63, and compile VTE with SIXEL support.
    --
    -- Note that even if you do the above, there may still be some problems
    -- with SIXEL support in VTE. Follow
    -- <https://gitlab.gnome.org/GNOME/vte/-/issues/253> for more information.
  , allowBold :: !Bool
    -- ^ Allow terminal to use bold text.
    --
    -- You may want to disable this, for instance, if you use a font that
    -- doesn't look good when bold.
  } deriving (Eq, Generic, FromJSON, Show, ToJSON)

cursorBlinkModeToString :: CursorBlinkMode -> Text
cursorBlinkModeToString = \case
  CursorBlinkModeSystem -> "system"
  CursorBlinkModeOn -> "on"
  CursorBlinkModeOff -> "off"
  AnotherCursorBlinkMode _ -> "other"

cursorBlinkModeFromString :: Text -> Maybe CursorBlinkMode
cursorBlinkModeFromString = \case
  "system" -> Just CursorBlinkModeSystem
  "on" -> Just CursorBlinkModeOn
  "off" -> Just CursorBlinkModeOff
  _ -> Nothing

instance FromJSON CursorBlinkMode where
  parseJSON = withText "CursorBlinkMode" $ \c -> do
    case (c :: Text) of
      "CursorBlinkModeSystem" -> pure CursorBlinkModeSystem
      "CursorBlinkModeOn" -> pure CursorBlinkModeOn
      "CursorBlinkModeOff" -> pure CursorBlinkModeOff
      _ -> fail "Wrong value for CursorBlinkMode"

instance ToJSON CursorBlinkMode where
  toJSON CursorBlinkModeSystem = String "CursorBlinkModeSystem"
  toJSON CursorBlinkModeOn = String "CursorBlinkModeOn"
  toJSON CursorBlinkModeOff = String "CursorBlinkModeOff"
  -- Not supposed to happened fall back to system
  toJSON (AnotherCursorBlinkMode _) = String "CursorBlinkModeSystem"

-- | The default 'ConfigOptions'.
--
-- >>> :{
--   let defConfOpt =
--         ConfigOptions
--           { fontConfig = defaultFontConfig
--           , showScrollbar = ShowScrollbarIfNeeded
--           , scrollbackLen = 10000
--           , confirmExit = True
--           , wordCharExceptions = "-#%&+,./=?@\\_~\183:"
--           , showMenu = True
--           , showTabBar = ShowTabBarIfNeeded
--           , cursorBlinkMode = CursorBlinkModeOn
--           , boldIsBright = False
--           , enableSixel = False
--           , allowBold = True
--           }
--   in defaultConfigOptions == defConfOpt
-- :}
-- True
defaultConfigOptions :: ConfigOptions
defaultConfigOptions =
  ConfigOptions
    { fontConfig = defaultFontConfig
    , showScrollbar = ShowScrollbarIfNeeded
    , scrollbackLen = 10000
    , confirmExit = True
    , wordCharExceptions = "-#%&+,./=?@\\_~\183:"
    , showMenu = True
    , showTabBar = ShowTabBarIfNeeded
    , cursorBlinkMode = CursorBlinkModeOn
    , boldIsBright = False
    , enableSixel = False
    , allowBold = True
    }

-- | The Termonad 'ConfigOptions' along with the 'ConfigHooks'.
data TMConfig = TMConfig
  { options :: !ConfigOptions
  , hooks :: !ConfigHooks
  } deriving Show

-- | The default 'TMConfig'.
--
-- 'options' is 'defaultConfigOptions' and 'hooks' is 'defaultConfigHooks'.
defaultTMConfig :: TMConfig
defaultTMConfig =
  TMConfig
    { options = defaultConfigOptions
    , hooks = defaultConfigHooks
    }

---------------------
-- ConfigHooks --
---------------------

-- | Hooks into certain termonad operations and VTE events. Used to modify
-- termonad's behaviour in order to implement new functionality. Fields should
-- have sane @Semigroup@ and @Monoid@ instances so that config extensions can
-- be combined uniformly and new hooks can be added without incident.
newtype ConfigHooks = ConfigHooks {
  -- | Produce an IO action to run on creation of new @Terminal@, given @TMState@
  -- and the @Terminal@ in question.
  createTermHook :: TMState -> Terminal -> IO ()
}

instance Show ConfigHooks where
  showsPrec :: Int -> ConfigHooks -> ShowS
  showsPrec _ _ =
    showString "ConfigHooks {" .
    showString "createTermHook = <function>" .
    showString "}"

-- | Default values for the 'ConfigHooks'.
--
-- - The default function for 'createTermHook' is 'defaultCreateTermHook'.
defaultConfigHooks :: ConfigHooks
defaultConfigHooks =
  ConfigHooks
    { createTermHook = defaultCreateTermHook
    }

-- | Default value for 'createTermHook'.  Does nothing.
defaultCreateTermHook :: TMState -> Terminal -> IO ()
defaultCreateTermHook _ _ = pure ()

----------------
-- Invariants --
----------------

data FocusNotSameErr
  = FocusListFocusExistsButNoNotebookTabWidget
  | NotebookTabWidgetDiffersFromFocusListFocus
  | NotebookTabWidgetExistsButNoFocusListFocus
  deriving Show

data TabsDoNotMatch
  = TabLengthsDifferent Int Int -- ^ The first 'Int' is the number of tabs in the
                                -- actual GTK 'Notebook'.  The second 'Int' is
                                -- the number of tabs in the 'FocusList'.
  | TabAtIndexDifferent Int     -- ^ The tab at index 'Int' is different between
                                -- the actual GTK 'Notebook' and the 'FocusList'.
  deriving (Show)

-- | An invariant error on a given 'TMWindow'.
data TMWinInvariantErr
  = FocusNotSame FocusNotSameErr Int
  | TabsDoNotMatch TabsDoNotMatch
  deriving Show

-- | An invariant error on a whole 'TMState'.
data TMStateInvariantErr
  = TMWinInvariantErr Int TMWinInvariantErr
    -- ^ An invariant error with a given 'TMWindow'.  The 'Int' indicates
    -- which window it is as an index into 'tmStateWindows'.
  deriving Show

invariantTMWindow :: TMWindow -> IO [TMWinInvariantErr]
invariantTMWindow tmWin =
  runInvariants
    [ invariantFocusSame
    , invariantTMTabLength
    , invariantTabsAllMatch
    ]
  where
    runInvariants :: [IO (Maybe TMWinInvariantErr)] -> IO [TMWinInvariantErr]
    runInvariants = fmap catMaybes . sequence

    invariantFocusSame :: IO (Maybe TMWinInvariantErr)
    invariantFocusSame = do
      let tmNote = tmNotebook $ tmWindowNotebook tmWin
      index32 <- notebookGetCurrentPage tmNote
      maybeWidgetFromNote <- notebookGetNthPage tmNote index32
      let focusList = tmNotebookTabs $ tmWindowNotebook tmWin
          maybeScrollWinFromFL =
            tmNotebookTabTermContainer <$> getFocusItemFL focusList
          idx = fromIntegral index32
      case (maybeWidgetFromNote, maybeScrollWinFromFL) of
        (Nothing, Nothing) -> pure Nothing
        (Just _, Nothing) ->
          pure $
            Just $
              FocusNotSame NotebookTabWidgetExistsButNoFocusListFocus idx
        (Nothing, Just _) ->
          pure $
            Just $
              FocusNotSame FocusListFocusExistsButNoNotebookTabWidget idx
        (Just widgetFromNote, Just scrollWinFromFL) -> do
          isEq <- widgetEq widgetFromNote scrollWinFromFL
          if isEq
            then pure Nothing
            else
              pure $
                Just $
                  FocusNotSame NotebookTabWidgetDiffersFromFocusListFocus idx

    invariantTMTabLength :: IO (Maybe TMWinInvariantErr)
    invariantTMTabLength = do
      let tmNote = tmNotebook $ tmWindowNotebook tmWin
      noteLength32 <- notebookGetNPages tmNote
      let noteLength = fromIntegral noteLength32
          focusListLength = lengthFL $ tmNotebookTabs $ tmWindowNotebook tmWin
          lengthEqual = focusListLength == noteLength
      if lengthEqual
        then pure Nothing
        else  pure $
               Just $
                TabsDoNotMatch $
                 TabLengthsDifferent noteLength focusListLength

    -- Turns a FocusList and Notebook into two lists of widgets and compares each widget for equality
    invariantTabsAllMatch :: IO (Maybe TMWinInvariantErr)
    invariantTabsAllMatch = do
      let tmNote = tmNotebook $ tmWindowNotebook tmWin
          focusList = tmNotebookTabs $ tmWindowNotebook tmWin
          flList = tmNotebookTabTermContainer <$> toList focusList
      noteList <- notebookToList tmNote
      tabsMatch noteList flList
      where
        tabsMatch
          :: forall a b
           . (IsWidget a, IsWidget b)
          => [a]
          -> [b]
          -> IO (Maybe TMWinInvariantErr)
        tabsMatch xs ys = foldr go (pure Nothing) (zip3 xs ys [0..])
          where
            go :: (a, b, Int) -> IO (Maybe TMWinInvariantErr) -> IO (Maybe TMWinInvariantErr)
            go (x, y, i) acc = do
              isEq <- widgetEq x y
              if isEq
                then acc
                else pure . Just $ TabsDoNotMatch (TabAtIndexDifferent i)

-- | Gather up the invariants for 'TMState' and return them as a list.
--
-- If no invariants have been violated, then this function should return an
-- empty list.
invariantTMState' :: TMState' -> IO [TMStateInvariantErr]
invariantTMState' tmState = do
  let tmWindows = tmStateWindows tmState
  ifoldMap go tmWindows
  where
    go :: Int -> TMWindow -> IO [TMStateInvariantErr]
    go idx tmWin = do
      tmWinErrs <- invariantTMWindow tmWin
      pure $ fmap (TMWinInvariantErr idx) tmWinErrs

-- | Check the invariants for 'TMState', and call 'fail' if we find that they
-- have been violated.
assertInvariantTMState :: TMState -> IO ()
assertInvariantTMState mvarTMState = do
  tmState <- readMVar mvarTMState
  assertValue <- invariantTMState' tmState
  case assertValue of
    [] x-> pure ()
    errs@(_:_) -> do
      putStrLn "In assertInvariantTMState, some invariants for TMState are being violated."
      putStrLn "\nInvariants violated:"
      print errs
      putStrLn "\nTMState:"
      pPrint tmState
      putStrLn ""
      fail "Invariants violated for TMState"

pPrintTMState :: TMState -> IO ()
pPrintTMState mvarTMState = do
  tmState <- readMVar mvarTMState
  pPrint tmState

traceShowMTMState :: TMState -> IO ()
traceShowMTMState mvarTMState = do
  tmState <- readMVar mvarTMState
  print tmState
