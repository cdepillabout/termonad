{-# LANGUAGE StandaloneDeriving #-}

module Termonad.Types where

import Termonad.Prelude

import Data.Unique (Unique, hashUnique, newUnique)
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
import GI.Pango (FontDescription)
import GI.Vte (Terminal, CursorBlinkMode)
import Text.Pretty.Simple (pPrint)
import Text.Show (Show(showsPrec), ShowS, showParen, showString)

import Termonad.FocusList (FocusList, emptyFL, singletonFL, getFLFocusItem, focusListLen)
import Termonad.Gtk (widgetEq)

data TMTerm = TMTerm
  { term :: !Terminal
  , pid :: !Int
  , unique :: !Unique
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

data TMNotebookTab = TMNotebookTab
  { tmNotebookTabTermContainer :: !ScrolledWindow
  , tmNotebookTabTerm :: !TMTerm
  , tmNotebookTabLabel :: !Label
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

data TMNotebook = TMNotebook
  { tmNotebook :: !Notebook
  , tmNotebookTabs :: !(FocusList TMNotebookTab)
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

data UserRequestedExit
  = UserRequestedExit
  | UserDidNotRequestExit
  deriving (Eq, Show)

data TMState' = TMState
  { tmStateApp :: !Application
  , tmStateAppWin :: !ApplicationWindow
  , tmStateNotebook :: !TMNotebook
  , tmStateFontDesc :: !FontDescription
  , tmStateConfig :: !TMConfig
  , tmStateUserReqExit :: !UserRequestedExit
  -- ^ This signifies whether or not the user has requested that Termonad
  -- exit by either closing all terminals or clicking the exit button.  If so,
  -- 'tmStateUserReqExit' should have a value of 'UserRequestedExit'.  However,
  -- if the window manager requested Termonad to exit (probably through the user
  -- trying to close Termonad through their window manager), then this will be
  -- set to 'UserDidNotRequestExit'.
  }

instance Show TMState' where
  showsPrec :: Int -> TMState' -> ShowS
  showsPrec d TMState{..} =
    showParen (d > 10) $
      showString "TMState {" .
      showString "tmStateApp = " .
      showString "(GI.GTK.Application)" .
      showString ", " .
      showString "tmStateAppWin = " .
      showString "(GI.GTK.ApplicationWindow)" .
      showString ", " .
      showString "tmStateNotebook = " .
      showsPrec (d + 1) tmStateNotebook .
      showString ", " .
      showString "tmStateFontDesc = " .
      showString "(GI.Pango.FontDescription)" .
      showString ", " .
      showString "tmStateConfig = " .
      showsPrec (d + 1) tmStateConfig .
      showString ", " .
      showString "tmStateUserReqExit = " .
      showsPrec (d + 1) tmStateUserReqExit .
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
newTMTerm trm pd = do
  unq <- newUnique
  pure $ createTMTerm trm pd unq

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

newTMState :: TMConfig -> Application -> ApplicationWindow -> TMNotebook -> FontDescription -> IO TMState
newTMState tmConfig app appWin note fontDesc =
  newMVar $
    TMState
      { tmStateApp = app
      , tmStateAppWin = appWin
      , tmStateNotebook = note
      , tmStateFontDesc = fontDesc
      , tmStateConfig = tmConfig
      , tmStateUserReqExit = UserDidNotRequestExit
      }

newEmptyTMState :: TMConfig -> Application -> ApplicationWindow -> Notebook -> FontDescription -> IO TMState
newEmptyTMState tmConfig app appWin note fontDesc =
  newMVar $
    TMState
      { tmStateApp = app
      , tmStateAppWin = appWin
      , tmStateNotebook = createEmptyTMNotebook note
      , tmStateFontDesc = fontDesc
      , tmStateConfig = tmConfig
      , tmStateUserReqExit = UserDidNotRequestExit
      }

newTMStateSingleTerm ::
     TMConfig
  -> Application
  -> ApplicationWindow
  -> Notebook
  -> Label
  -> ScrolledWindow
  -> Terminal
  -> Int
  -> FontDescription
  -> IO TMState
newTMStateSingleTerm tmConfig app appWin note label scrollWin trm pd fontDesc = do
  tmTerm <- newTMTerm trm pd
  let tmNoteTab = createTMNotebookTab label scrollWin tmTerm
      tabs = singletonFL tmNoteTab
      tmNote = createTMNotebook note tabs
  newTMState tmConfig app appWin tmNote fontDesc

traceShowMTMState :: TMState -> IO ()
traceShowMTMState mvarTMState = do
  tmState <- readMVar mvarTMState
  print tmState

------------
-- Config --
------------

data TMConfig = TMConfig
  { fontConfig :: !FontConfig
  , showScrollbar :: !ShowScrollbar
  , scrollbackLen :: !Integer
  , confirmExit :: !Bool
  , wordCharExceptions :: !Text
  , showMenu :: !Bool
  , showTabBar :: !ShowTabBar
  , cursorBlinkMode :: !CursorBlinkMode
  , extension :: !SomeConfigExtension
  } deriving (Eq, Show)

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
  deriving (Eq, Show)

-- | Settings for the font to be used in Termonad.
data FontConfig = FontConfig
  { fontFamily :: !Text
    -- ^ The font family to use.  Example: @"DejaVu Sans Mono"@ or @"Source Code Pro"@
  , fontSize :: !FontSize
    -- ^ The font size.
  } deriving (Eq, Show)

-- | This data type represents an option that can either be 'Set' or 'Unset'.
--
-- This data type is used in situations where leaving an option unset results
-- in a special state that is not representable by setting any specific value.
--
-- Examples of this include the 'cursorFgColour' and 'cursorBgColour' options
-- supplied by the 'ColourConfig' @ConfigExtension@.  By default,
-- 'cursorFgColour' and 'cursorBgColour' are both 'Unset'.  However, when
-- 'cursorBgColour' is 'Set', 'cursorFgColour' defaults to the color of the text
-- underneath.  There is no way to represent this by setting 'cursorFgColour'.
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
  Unset -> \_ -> mempty
  Set x -> \f -> f x

data ShowScrollbar
  = ShowScrollbarNever
  | ShowScrollbarAlways
  | ShowScrollbarIfNeeded
  deriving (Eq, Show)

data ShowTabBar
  = ShowTabBarNever
  | ShowTabBarAlways
  | ShowTabBarIfNeeded
  deriving (Eq, Show)

---------------------
-- ConfigExtension --
---------------------

-- | Hooks into certain termonad operations and VTE events. Used to modify
--   termonad's behaviour in order to implement new functionality. Fields should
--   have sane @Semigroup@ and @Monoid@ instances so that config extensions can
--   be combined uniformly and new hooks can be added without incident.
data ConfigHooks = ConfigHooks {
  -- | Produce an IO action to run on creation of new @Terminal@, given @TMState@
  --   and the @Terminal@ in question.
  createTermHook :: TMState -> Terminal -> IO ()
}

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

-- | A class for data types that can be sent to and caught by config extensions.
class Typeable m => Message m

-- | The @ConfigExtension@ class can be used to extend termonad from your
--   configuration file. A data type holding the desired configuration options
--   can be made a @ConfigExtension@ instance by declaring how to produce the
--   correct set of 'ConfigHooks' from those options, then the new options can be
--   used by adding them to your 'TMConfig' with '<+>'.
--
--   Optionally the extended capabilities can include runtime interaction if
--   messages are defined and the @message@ class method is implemented.
class ConfigExtension g where

  -- | Produce hooks from config options.
  hooks :: g -> ConfigHooks

  -- | Catch messages and act on them, returning a potentially changed config.
  message :: Message m => TMState -> m -> g -> IO g
  message _ _ g = pure g

-- | A datatype to use in 'defaultConfigExtension'.  This is used as a default
-- extension that does nothing.  It is used as the base for other extensions.
data NoExtensions = NoExtensions deriving Show

instance ConfigExtension NoExtensions where
  hooks NoExtensions = defaultConfigHooks

-- | An existential data type over config extensions. Is used internally by
--   @TMConfig@ and needed neither by the end user nor the extension implementor.
data SomeConfigExtension = forall g. ConfigExtension g => SomeConfigExtension g

instance Show SomeConfigExtension where
  show _ = "SomeConfigExtension"

instance Eq SomeConfigExtension where
  _ == _ = True

instance ConfigExtension SomeConfigExtension where
  hooks (SomeConfigExtension g) = hooks g
  message mvarTMState m (SomeConfigExtension g) =
    SomeConfigExtension <$> message mvarTMState m g

defaultConfigExtension :: SomeConfigExtension
defaultConfigExtension = SomeConfigExtension NoExtensions

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

data TMStateInvariantErr
  = FocusNotSame FocusNotSameErr Int
  | TabsDoNotMatch TabsDoNotMatch
  deriving Show

-- | Gather up the invariants for 'TMState' and return them as a list.
--
-- If no invariants have been violated, then this function should return an
-- empty list.
invariantTMState' :: TMState' -> IO [TMStateInvariantErr]
invariantTMState' tmState =
  runInvariants
    [ invariantFocusSame
    , invariantTMTabLength
    , invariantTabsAllMatch
    ]
  where
    runInvariants :: [IO (Maybe TMStateInvariantErr)] -> IO [TMStateInvariantErr]
    runInvariants = fmap catMaybes . sequence

    invariantFocusSame :: IO (Maybe TMStateInvariantErr)
    invariantFocusSame = do
      let tmNote = tmNotebook $ tmStateNotebook tmState
      index32 <- notebookGetCurrentPage tmNote
      maybeWidgetFromNote <- notebookGetNthPage tmNote index32
      let focusList = tmNotebookTabs $ tmStateNotebook tmState
          maybeScrollWinFromFL =
            fmap tmNotebookTabTermContainer $ getFLFocusItem $ focusList
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

    invariantTMTabLength :: IO (Maybe TMStateInvariantErr)
    invariantTMTabLength = do
      let tmNote = tmNotebook $ tmStateNotebook tmState
      noteLength32 <- notebookGetNPages tmNote
      let noteLength = fromIntegral noteLength32
          focusListLength = focusListLen $ tmNotebookTabs $ tmStateNotebook tmState
          lengthEqual = focusListLength == noteLength
      if lengthEqual
        then pure Nothing
        else  pure $
               Just $
                TabsDoNotMatch $
                 TabLengthsDifferent noteLength focusListLength

    -- Turns a FocusList and Notebook into two lists of widgets and compares each widget for equality
    invariantTabsAllMatch :: IO (Maybe TMStateInvariantErr)
    invariantTabsAllMatch = do
      let tmNote = tmNotebook $ tmStateNotebook tmState
          focusList = tmNotebookTabs $ tmStateNotebook tmState
          flList = fmap tmNotebookTabTermContainer $ toList focusList
      noteList <- notebookToList tmNote
      tabsMatch noteList flList
      where
        tabsMatch
          :: forall a b
           . (IsWidget a, IsWidget b)
          => [a]
          -> [b]
          -> IO (Maybe TMStateInvariantErr)
        tabsMatch xs ys = foldr go (pure Nothing) (zip3 xs ys [0..])
          where
            go :: (a, b, Int) -> IO (Maybe TMStateInvariantErr) -> IO (Maybe TMStateInvariantErr)
            go (x, y, i) acc = do
              isEq <- widgetEq x y
              if isEq
                then acc
                else pure . Just $ TabsDoNotMatch (TabAtIndexDifferent i)

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
