{-# LANGUAGE TemplateHaskell #-}

module Termonad.Lenses where

import Termonad.Prelude

import Control.Lens (Lens', Traversal', makeLensesFor, makePrisms)
import Termonad.Types

$(makeLensesFor
    [ ("tmTermScrolledWindow", "lensTMTermScrolledWindow")
    , ("term", "lensTerm")
    , ("pid", "lensPid")
    , ("unique", "lensUnique")
    ]
    ''TMTerm
 )

$(makeLensesFor
    [ ("tmNotebookTabPaned", "lensTMNotebookTabPaned")
    , ("tmNotebookTabLeftTerm", "lensTMNotebookTabLeftTerm")
    , ("tmNotebookTabRightTerm", "lensTMNotebookTabRightTerm")
    , ("tmNotebookTabLabel", "lensTMNotebookTabLabel")
    ]
    ''TMNotebookTab
 )

lensTMNotebookTabFocusedTerm :: Lens' TMNotebookTab TMTerm
lensTMNotebookTabFocusedTerm f notebookTab
  = if tmNotebookTabFocusIsOnLeft notebookTab
    then lensTMNotebookTabLeftTerm f notebookTab
    else lensTMNotebookTabRightTerm f notebookTab

traversalTMNotebookTabTerms :: Traversal' TMNotebookTab TMTerm
traversalTMNotebookTabTerms f notebookTab
    = (\termL termR -> notebookTab
                         { tmNotebookTabLeftTerm = termL
                         , tmNotebookTabRightTerm = termR
                         })
  <$> f (tmNotebookTabLeftTerm notebookTab)
  <*> f (tmNotebookTabRightTerm notebookTab)

$(makeLensesFor
    [ ("tmNotebook", "lensTMNotebook")
    , ("tmNotebookTabs", "lensTMNotebookTabs")
    ]
    ''TMNotebook
 )

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

$(makePrisms ''FontSize)

$(makeLensesFor
    [ ("fontFamily", "lensFontFamily")
    , ("fontSize", "lensFontSize")
    ]
    ''FontConfig
 )

$(makeLensesFor
    [ ("fontConfig", "lensFontConfig")
    , ("showScrollbar", "lensShowScrollbar")
    , ("colourConfig", "lensColourConfig")
    , ("scrollbackLen", "lensScrollbackLen")
    , ("confirmExit", "lensConfirmExit")
    , ("wordCharExceptions", "lensWordCharExceptions")
    , ("showMenu", "lensShowMenu")
    , ("showTabBar", "lensShowTabBar")
    , ("cursorBlinkMode", "lensCursorBlinkMode")
    , ("boldIsBright", "lensBoldIsBright")
    ]
    ''ConfigOptions
 )

$(makeLensesFor
    [ ("createTermHook", "lensCreateTermHook")
    ]
    ''ConfigHooks
 )

$(makeLensesFor
    [ ("options", "lensOptions")
    , ("hooks", "lensHooks")
    ]
    ''TMConfig
 )
