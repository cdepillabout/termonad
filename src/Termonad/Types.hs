{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Termonad.Types where

import Termonad.Prelude

import Control.Lens ((&), (.~), (^.), firstOf, makeLensesFor)
import Data.Unique (Unique, hashUnique, newUnique)
import GI.Gtk
  ( Application
  , ApplicationWindow
  , Label
  , Notebook
  , ScrolledWindow
  , IsWidget
  , Widget
  , notebookGetCurrentPage
  , notebookGetNthPage
  , notebookGetNPages
  )
import GI.Pango (FontDescription)
import GI.Vte (Terminal)
import Text.Pretty.Simple (pPrint)
import Text.Show (Show(showsPrec), ShowS, showParen, showString)
import Data.Maybe(fromJust)

import Termonad.Config (TMConfig)
import Termonad.FocusList (FocusList, emptyFL, focusItemGetter, singletonFL, getFLFocusItem, focusListLen, toListFL)
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

$(makeLensesFor
    [ ("term", "lensTerm")
    , ("pid", "lensPid")
    , ("unique", "lensUnique")
    ]
    ''TMTerm
 )

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

$(makeLensesFor
    [ ("tmNotebookTabTermContainer", "lensTMNotebookTabTermContainer")
    , ("tmNotebookTabTerm", "lensTMNotebookTabTerm")
    , ("tmNotebookTabLabel", "lensTMNotebookTabLabel")
    ]
    ''TMNotebookTab
 )

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

$(makeLensesFor
    [ ("tmNotebook", "lensTMNotebook")
    , ("tmNotebookTabs", "lensTMNotebookTabs")
    ]
    ''TMNotebook
 )

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

$(makeLensesFor
    [ ("tmStateApp", "lensTMStateApp")
    , ("tmStateAppWin", "lensTMStateAppWin")
    , ("tmStateNotebook", "lensTMStateNotebook")
    , ("tmStateFontDesc", "lensTMStateFontDesc")
    , ("tmStateConfig", "lensTMStateConfig")
    , ("tmStateUserReqExit", "lensTMStateUserReqExit")
    ]
    ''TMState'
 )

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
  let foldHelper :: Int32 -> [Widget] -> IO [Widget]
      foldHelper index32 acc = do
        notePage <- notebookGetNthPage notebook index32
        case notePage of
          Nothing -> pure acc
          _  -> foldHelper (index32 + 1) (acc ++ [fromJust notePage]) 
  in            
    foldHelper 0 []
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

data FocusNotSameErr
  = FocusListFocusExistsButNoNotebookTabWidget
  | NotebookTabWidgetDiffersFromFocusListFocus
  | NotebookTabWidgetExistsButNoFocusListFocus
  deriving Show

data TabsDoNotMatch =
  TabLengthsDifferent Int Int -- ^ The first 'Int' is the number of tabs in the actual GTK 'Notebook'.  The second 'Int' is the number of tabs in the 'FocusList'.
  |
  TabAtIndexDifferent Int -- ^ The tab at index 'Int' is different between the actual GTK 'Notebook' and the 'FocusList'.
  deriving Show

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
    [   invariantFocusSame
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
      case lengthEqual of
        True  -> pure Nothing
        False -> pure $
                  Just $
                   TabsDoNotMatch $
                    TabLengthsDifferent noteLength focusListLength

    invariantTabsAllMatch :: IO (Maybe TMStateInvariantErr)
    invariantTabsAllMatch = do
      --Turns a FocusList and Notebook into two lists of widgets and compares each widget for equality
      let tmNote = tmNotebook $ tmStateNotebook tmState
          focusList = tmNotebookTabs $ tmStateNotebook tmState
          flList = fmap tmNotebookTabTermContainer $ toListFL focusList
      noteList <- notebookToList tmNote
      tabsMatch noteList flList 0
      where
        tabsMatch :: (IsWidget a, IsWidget b) => [a] -> [b] -> Int -> IO (Maybe TMStateInvariantErr)        
        tabsMatch [] [] _ = pure Nothing
        tabsMatch [] (_:_) _ = pure Nothing
        tabsMatch (_:_) [] _ = pure Nothing
        tabsMatch (x:xs) (y:ys) i = do
          isEq <- widgetEq x y
          if isEq
          then tabsMatch xs ys (i+1)
          else pure $
                      Just $
                       TabsDoNotMatch $
                        TabAtIndexDifferent i

-- | Check the invariants for 'TMState', and call 'fail' if we find that they
-- have been violated.
assertInvariantTMState :: TMState -> IO ()
assertInvariantTMState mvarTMState = do
  tmState <- readMVar mvarTMState
  assertValue <- invariantTMState' tmState
  case assertValue of
    [] -> pure ()
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

getFocusedTermFromState :: TMState -> IO (Maybe Terminal)
getFocusedTermFromState mvarTMState = do
  withMVar
    mvarTMState
    ( pure .
      firstOf
        ( lensTMStateNotebook .
          lensTMNotebookTabs .
          focusItemGetter .
          traverse .
          lensTMNotebookTabTerm .
          lensTerm
        )
    )

setUserRequestedExit :: TMState -> IO ()
setUserRequestedExit mvarTMState = do
  modifyMVar_ mvarTMState $ \tmState -> do
    pure $ tmState & lensTMStateUserReqExit .~ UserRequestedExit

getUserRequestedExit :: TMState -> IO UserRequestedExit
getUserRequestedExit mvarTMState = do
  tmState <- readMVar mvarTMState
  pure $ tmState ^. lensTMStateUserReqExit
