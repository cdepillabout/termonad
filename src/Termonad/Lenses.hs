{-# LANGUAGE TemplateHaskell #-}

module Termonad.Lenses where

import Control.Lens (makeLensesFor, makePrisms)
import Termonad.Types

$(makeLensesFor
    [ ("term", "lensTerm")
    , ("pid", "lensPid")
    , ("unique", "lensUnique")
    ]
    ''TMTerm
 )

$(makeLensesFor
    [ ("tmNotebookTabScrolledWindow", "lensTMNotebookTabScrolledWindow")
    , ("tmNotebookTabTerm", "lensTMNotebookTabTerm")
    , ("tmNotebookTabLabel", "lensTMNotebookTabLabel")
    ]
    ''TMNotebookTab
 )

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
