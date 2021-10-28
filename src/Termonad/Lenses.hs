{-# LANGUAGE TemplateHaskell #-}

module Termonad.Lenses where

import Termonad.Prelude

import Control.Lens (Lens', Traversal', makeLensesFor, makePrisms)
import Data.FocusList (FocusList)
import Termonad.Types
import qualified Data.FocusList as FocusList
import qualified Data.Maybe as Unsafe (fromJust)
import qualified Data.Sequence as Seq

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
    , ("tmNotebookTabFocusIsOnLeft", "lensTMNotebookTabFocusIsOnLeft")
    , ("tmNotebookTabLabel", "lensTMNotebookTabLabel")
    ]
    ''TMNotebookTab
 )

lensTMNotebookTabFocusedTerm :: Lens' TMNotebookTab TMTerm
lensTMNotebookTabFocusedTerm f notebookTab
  = if tmNotebookTabFocusIsOnLeft notebookTab
    then lensTMNotebookTabLeftTerm f notebookTab
    else lensTMNotebookTabRightTerm f notebookTab

lensTMNotebookTabNonFocusedTerm :: Lens' TMNotebookTab TMTerm
lensTMNotebookTabNonFocusedTerm f notebookTab
  = if tmNotebookTabFocusIsOnLeft notebookTab
    then lensTMNotebookTabRightTerm f notebookTab
    else lensTMNotebookTabLeftTerm f notebookTab

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

-- TODO: upstream this to the focuslist package
traversalFLItem :: forall a. Traversal' (FocusList a) a
traversalFLItem f flA
  = let seqA = FocusList.toSeqFL flA
        maybeFocus = FocusList.getFocus (FocusList.getFocusFL flA)
        maybeFocusItem = FocusList.getFocusItemFL flA
    in case (maybeFocus, maybeFocusItem) of
         (Just i, Just a)
           -> let makeUpdatedFL :: a -> FocusList a
                  makeUpdatedFL a'
                    = Unsafe.fromJust  -- safe because i and the length are unchanged
                    $ FocusList.fromFoldableFL
                        (FocusList.Focus i)
                        (Seq.update i a' seqA)
              in makeUpdatedFL <$> f a
         _
           -> pure flA

traversalTMNotebookFocusedTab :: Traversal' TMNotebook TMNotebookTab
traversalTMNotebookFocusedTab = lensTMNotebookTabs . traversalFLItem

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
