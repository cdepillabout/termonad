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
  , notebookGetCurrentPage
  , notebookGetNthPage
  , toWidget
  )
import GI.Gtk (ManagedPtr(..))
import GI.Gtk (Widget(..))
import GI.Pango (FontDescription)
import GI.Vte (Terminal)
import Text.Pretty.Simple (pPrint)
import Text.Show (Show(showsPrec), ShowS, showParen, showString)

import Termonad.Config (TMConfig)
import Termonad.FocusList (FocusList, emptyFL, focusItemGetter, singletonFL, getFLFocusItem, unsafeGetFLFocusItem)

import Data.Maybe (fromMaybe,isNothing,fromJust)

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

data UserRequestedExit = UserRequestedExit | UserDidNotRequestExit deriving (Eq, Show)

-- A hack to get widgets to have an eq class
instance Eq (ManagedPtr a) where
    (==) ManagedPtr { managedForeignPtr = p  }
         ManagedPtr { managedForeignPtr = p' }
         = p == p'

deriving instance Eq Widget

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
--checks that the FocusList and the actual gtk widget focused are in agreement.
--Most of what is happening here is just stripping down so they are the same type
invariantTMState :: TMState' -> IO Bool
invariantTMState tmState = do
    let tmNote = tmNotebook $ tmStateNotebook tmState
    index <- notebookGetCurrentPage tmNote
    noteWidget <- notebookGetNthPage tmNote index
    let focusList = tmNotebookTabs $ tmStateNotebook tmState
    let scrollWinFL = fmap tmNotebookTabTermContainer $ getFLFocusItem $ focusList
    --This bit is necessary to get past initial termCreate
    if (isNothing scrollWinFL )
      then return True
      else
          do widgetFL <- toWidget $ fromJust scrollWinFL
             return (noteWidget == (Just widgetFL))

assertTMStateInvariant :: TMState' -> IO ()
assertTMStateInvariant tmState = do
   assertValue <- invariantTMState tmState
   if (assertValue == True)
      then return ()
      else error $ pack "FocusList and Notebook do not match!"
          
  

pTraceShowMTMState :: TMState -> IO ()
pTraceShowMTMState mvarTMState = do
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
