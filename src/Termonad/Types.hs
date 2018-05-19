
module Termonad.Types where

import Termonad.FocusedList

data TMTerm = TMTerm
  { term :: Terminal
  , unique :: Unique
  }

data TMNotebookTab = TMNotebookTab
  { tmNotebookTabTermContainer :: ScrolledWindow
  , tmNotebookTabTerm :: TMTerm
  }

data TMNotebook = TMNotebook
  { tmNotebook :: !Notebook
  , tmNotebookTabs :: !(FocusedList TMNotebookTab)
  }

data TMState' = TMState
  { tmStateApp :: !Application
  , tmStateAppWin :: !ApplicationWindow
  , tmStateNotebook :: !TMNotebook
  , tmStateFontDesc :: !FontDescription
  }

type TMState = MVar TMState'

instance Eq TMTerm where
  (==) :: TMTerm -> TMTerm -> Bool
  (==) = (==) `on` (unique :: TMTerm -> Unique)

createTMTerm :: Terminal -> Unique -> TMTerm
createTMTerm trm unq =
  TMTerm
    { term = trm
    , unique = unq
    }

newTMTerm :: Terminal -> IO TMTerm
newTMTerm trm =
  unq <- newUnique
  pure $
    TMTerm
      { term = trm
      , unique = unq
      }

createTMNotebookTab :: ScrolledWindow -> TMTerm -> TMNotebookTab
createTMNotebookTab scrollWin trm =
  TMNotebookTab
    { tmNotebookTabTermContainer = scrollWin
    , tmNotebookTabTerm = trm
    }

createTMNotebook :: Notebook -> FocusedList TMNotebookTab -> TMNotebook
createTMNotebook note tabs =
  TMNotebook
    { tmNotebook = note
    , tmNotebookTabs = tabs
    }

newTMState :: Application -> ApplicationWindow -> TMNotebook -> FontDescription -> IO TMState
newTMState app appWin note fontDesc =
  newMVar $
    TMState
      { tmStateApp = app
      , tmStateAppWin = appWin
      , tmStateNotebook = note
      , tmStateFontDesc = fontDesc
      }

newTMStateSingleTerm ::
     Application
  -> ApplicationWindow
  -> Notebook
  -> SrolledWindow
  -> Terminal
  -> FontDescription
  -> IO TMState
newTMStateSingleTerm app appWin note scrollWin trm fontDesc = do
  tmTerm <- newTMTerm trm
  let tmNoteTab = createTMNotebookTab scrollWin tmTerm
      tabs = singletonFL tmNoteTab
      tmNote = createTMNotebook note tabs
  newTMState app appWin tmNote fontDesc
