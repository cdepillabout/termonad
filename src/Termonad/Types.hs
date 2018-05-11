
module Termonad.Types where

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
