{-# LANGUAGE CPP #-}

-- | Description : Controls the Preferences dialog and setting app preferences
-- Copyright     : (c) Dennis Gosnell, 2023
-- License       : BSD3
-- Stability     : experimental
-- Portability   : POSIX
--
-- This module controls the Preferences dialog, which lets you set Termonad
-- preferences at run-time.
--
-- It also exports helpful functions from "Termonad.Preferences.File".

module Termonad.Preferences
  ( module Termonad.Preferences.File
  , showPreferencesDialog
  ) where

import Termonad.Prelude

import Control.Lens ((^.), over, set, view)
import qualified Data.List as List
import qualified Data.Text as Text
import GI.Gtk
  ( CheckButton(CheckButton)
  , ComboBoxText(ComboBoxText)
  , Dialog(Dialog)
  , Entry(Entry)
  , FontButton(FontButton)
  , Label(Label)
  , PolicyType(PolicyTypeAutomatic)
  , ResponseType(ResponseTypeAccept)
  , SpinButton(SpinButton)
  , adjustmentNew
  , applicationGetActiveWindow
  , applicationWindowSetShowMenubar
  , builderNewFromString
  , comboBoxGetActiveId
  , comboBoxSetActiveId
  , comboBoxTextAppend
  , dialogRun
  , entryBufferGetText
  , entryBufferSetText
  , fontChooserSetFontDesc
  , fontChooserGetFontDesc
  , getEntryBuffer
  , scrolledWindowSetPolicy
  , spinButtonGetValueAsInt
  , spinButtonSetAdjustment
  , spinButtonSetValue
  , toggleButtonGetActive
  , toggleButtonSetActive
  , widgetDestroy
  , widgetSetVisible
  , windowSetTransientFor
  )
import GI.Vte
  ( CursorBlinkMode(..)
  , terminalSetBoldIsBright
  , terminalSetCursorBlinkMode
  , terminalSetFont
  , terminalSetScrollbackLines
  , terminalSetWordCharExceptions
  , terminalSetAllowBold
  )
import System.Environment (getExecutablePath)
import System.FilePath (takeFileName)
import Termonad.Gtk (objFromBuildUnsafe, terminalSetEnableSixelIfExists)
import Termonad.Lenses
  ( lensBoldIsBright
  , lensEnableSixel
  , lensAllowBold
  , lensConfirmExit
  , lensCursorBlinkMode
  , lensFontConfig
  , lensOptions
  , lensShowMenu
  , lensShowScrollbar
  , lensShowTabBar
  , lensScrollbackLen
  , lensTMNotebookTabTermContainer
  , lensTMNotebookTabTerm
  , lensTMStateApp
  , lensTMStateConfig
  , lensTMStateFontDesc
  , lensTerm
  , lensWordCharExceptions
  )
import Termonad.Preferences.File (saveToPreferencesFile, tmConfigFromPreferencesFile)
import Termonad.Term
  ( setShowTabs
  , showScrollbarToPolicy
  )
import Termonad.Types
  ( ConfigOptions(..)
  , ShowScrollbar(..)
  , ShowTabBar(..)
  , TMNotebookTab
  , TMState
  , TMWindowId
  , fontConfigFromFontDescription
  , getTMWindowFromTMState'
  , tmNotebook
  , tmNotebookTabs
  , tmStateWindows
  , tmWindowAppWin
  , tmWindowNotebook
  )
import Termonad.XML (preferencesText)
import Termonad.IdMap (keysIdMap)

-- | Fill a combo box with ids and labels
--
-- The ids are stored in the combobox as 'Text', so their type should be an
-- instance of the 'Show' type class.
comboBoxFill :: forall a. Show a => ComboBoxText -> [(a, Text)] -> IO ()
comboBoxFill comboBox = mapM_ go
  where
    go :: (a, Text) -> IO ()
    go (value, textId) =
      comboBoxTextAppend comboBox (Just $ tshow value) textId

-- | Set the current active item in a combobox given an input id.
comboBoxSetActive :: Show a => ComboBoxText -> a -> IO ()
comboBoxSetActive cb item = void $ comboBoxSetActiveId cb (Just $ tshow item)

-- | Get the current active item in a combobox
--
-- The list of values to be searched in the combobox must be given as a
-- parameter. These values are converted to Text then compared to the current
-- id.
comboBoxGetActive
  :: forall a. (Show a, Enum a) => ComboBoxText -> [a] -> IO (Maybe a)
comboBoxGetActive cb values = findEnumFromMaybeId <$> comboBoxGetActiveId cb
  where
    findEnumFromMaybeId :: Maybe Text -> Maybe a
    findEnumFromMaybeId maybeId = maybeId >>= findEnumFromId

    findEnumFromId :: Text -> Maybe a
    findEnumFromId label = List.find (\x -> tshow x == label) values

applyNewPreferencesToTab :: TMState -> TMNotebookTab -> IO ()
applyNewPreferencesToTab mvarTMState tab = do
  tmState <- readMVar mvarTMState
  let fontDesc = tmState ^. lensTMStateFontDesc
      term = tab ^. lensTMNotebookTabTerm . lensTerm
      scrolledWin = tab ^. lensTMNotebookTabTermContainer
      options = tmState ^. lensTMStateConfig . lensOptions
  terminalSetFont term (Just fontDesc)
  terminalSetCursorBlinkMode term (cursorBlinkMode options)
  terminalSetWordCharExceptions term (wordCharExceptions options)
  terminalSetScrollbackLines term (fromIntegral (scrollbackLen options))
  terminalSetBoldIsBright term (boldIsBright options)
  terminalSetEnableSixelIfExists term (enableSixel options)
  terminalSetAllowBold term (allowBold options)

  let vScrollbarPolicy = showScrollbarToPolicy (options ^. lensShowScrollbar)
  scrolledWindowSetPolicy scrolledWin PolicyTypeAutomatic vScrollbarPolicy

applyNewPreferencesToWindow :: TMState -> TMWindowId -> IO ()
applyNewPreferencesToWindow mvarTMState tmWinId = do
  tmState <- readMVar mvarTMState
  tmWin <- getTMWindowFromTMState' tmState tmWinId
  let appWin = tmWindowAppWin tmWin
      config = tmState ^. lensTMStateConfig
      notebook = tmWindowNotebook tmWin
      tabFocusList = tmNotebookTabs notebook
      showMenu = config  ^. lensOptions . lensShowMenu
  applicationWindowSetShowMenubar appWin showMenu
  setShowTabs config (tmNotebook notebook)
  -- Sets the remaining preferences to each tab
  foldMap (applyNewPreferencesToTab mvarTMState) tabFocusList

-- | Takes a 'TMState', and looks at the 'TMConfig' within.
-- Take all the configuration options from the 'TMConfig' and apply them to the
-- current 'Application', 'Window's, and 'Term's.
--
-- This function is meant to be used after a big update to the 'TMConfig' within a
-- 'TMState'.
applyNewPreferences :: TMState -> IO ()
applyNewPreferences mvarTMState = do
  tmState <- readMVar mvarTMState
  let windows = tmStateWindows tmState
  foldMap (applyNewPreferencesToWindow mvarTMState) (keysIdMap windows)

-- | Show the preferences dialog.
--
-- When the user clicks on the Ok button, it copies the new settings to TMState.
-- Then apply them to the current terminals.
showPreferencesDialog :: TMState -> IO ()
showPreferencesDialog mvarTMState = do
  -- Get app out of mvar
  tmState <- readMVar mvarTMState
  let app = tmState ^. lensTMStateApp

  -- Create the preference dialog and get some widgets
  preferencesBuilder <-
    builderNewFromString preferencesText $ fromIntegral (Text.length preferencesText)
  preferencesDialog <-
    objFromBuildUnsafe preferencesBuilder "preferences" Dialog
  confirmExitCheckButton <-
    objFromBuildUnsafe preferencesBuilder "confirmExit" CheckButton
  showMenuCheckButton <-
    objFromBuildUnsafe preferencesBuilder "showMenu" CheckButton
  boldIsBrightCheckButton <-
    objFromBuildUnsafe preferencesBuilder "boldIsBright" CheckButton
  enableSixelCheckButton <-
    objFromBuildUnsafe preferencesBuilder "enableSixel" CheckButton
  allowBoldCheckButton <-
    objFromBuildUnsafe preferencesBuilder "allowBold" CheckButton
  wordCharExceptionsEntryBuffer <-
    objFromBuildUnsafe preferencesBuilder "wordCharExceptions" Entry >>=
      getEntryBuffer
  fontButton <- objFromBuildUnsafe preferencesBuilder "font" FontButton
  showScrollbarComboBoxText <-
    objFromBuildUnsafe preferencesBuilder "showScrollbar" ComboBoxText
  comboBoxFill
    showScrollbarComboBoxText
    [ (ShowScrollbarNever, "Never")
    , (ShowScrollbarAlways, "Always")
    , (ShowScrollbarIfNeeded, "If needed")
    ]
  showTabBarComboBoxText <-
    objFromBuildUnsafe preferencesBuilder "showTabBar" ComboBoxText
  comboBoxFill
    showTabBarComboBoxText
    [ (ShowTabBarNever, "Never")
    , (ShowTabBarAlways, "Always")
    , (ShowTabBarIfNeeded, "If needed")
    ]
  cursorBlinkModeComboBoxText <-
    objFromBuildUnsafe preferencesBuilder "cursorBlinkMode" ComboBoxText
  comboBoxFill
    cursorBlinkModeComboBoxText
    [ (CursorBlinkModeSystem, "System")
    , (CursorBlinkModeOn, "On")
    , (CursorBlinkModeOff, "Off")
    ]
  scrollbackLenSpinButton <-
    objFromBuildUnsafe preferencesBuilder "scrollbackLen" SpinButton
  adjustmentNew 0 0 (fromIntegral (maxBound :: Int)) 1 10 0 >>=
    spinButtonSetAdjustment scrollbackLenSpinButton
  warningLabel <- objFromBuildUnsafe preferencesBuilder "warning" Label

  -- We show the warning label only if the user has launched termonad with a
  -- termonad.hs file
  executablePath <- getExecutablePath
  let hasTermonadHs = takeFileName executablePath == "termonad-linux-x86_64"
  widgetSetVisible warningLabel hasTermonadHs

  -- Make the dialog modal
  maybeWin <- applicationGetActiveWindow app
  windowSetTransientFor preferencesDialog maybeWin

  -- Init with current state
  fontChooserSetFontDesc fontButton (tmState ^. lensTMStateFontDesc)
  let options = tmState ^. lensTMStateConfig . lensOptions
  comboBoxSetActive showScrollbarComboBoxText $ showScrollbar options
  comboBoxSetActive showTabBarComboBoxText $ showTabBar options
  comboBoxSetActive cursorBlinkModeComboBoxText $ cursorBlinkMode options
  spinButtonSetValue scrollbackLenSpinButton (fromIntegral $ scrollbackLen options)
  toggleButtonSetActive confirmExitCheckButton $ confirmExit options
  toggleButtonSetActive showMenuCheckButton $ showMenu options
  toggleButtonSetActive boldIsBrightCheckButton $ boldIsBright options
  toggleButtonSetActive enableSixelCheckButton $ enableSixel options
  toggleButtonSetActive allowBoldCheckButton $ allowBold options
  entryBufferSetText wordCharExceptionsEntryBuffer (wordCharExceptions options) (-1)

  -- Run dialog then close
  res <- dialogRun preferencesDialog

  -- When closing the dialog get the new settings
  when (toEnum (fromIntegral res) == ResponseTypeAccept) $ do
    maybeFontDesc <- fontChooserGetFontDesc fontButton
    maybeFontConfig <-
      join <$> mapM fontConfigFromFontDescription maybeFontDesc
    maybeShowScrollbar <-
      comboBoxGetActive showScrollbarComboBoxText [ShowScrollbarNever ..]
    maybeShowTabBar <-
      comboBoxGetActive showTabBarComboBoxText [ShowTabBarNever ..]
    maybeCursorBlinkMode <-
      comboBoxGetActive cursorBlinkModeComboBoxText [CursorBlinkModeSystem ..]
    scrollbackLenVal <-
      fromIntegral <$> spinButtonGetValueAsInt scrollbackLenSpinButton
    confirmExitVal <- toggleButtonGetActive confirmExitCheckButton
    showMenuVal <- toggleButtonGetActive showMenuCheckButton
    boldIsBrightVal <- toggleButtonGetActive boldIsBrightCheckButton
    enableSixelVal <- toggleButtonGetActive enableSixelCheckButton
    allowBoldVal <- toggleButtonGetActive allowBoldCheckButton
    wordCharExceptionsVal <- entryBufferGetText wordCharExceptionsEntryBuffer

    -- Apply the changes to mvarTMState
    modifyMVar_ mvarTMState $ pure
      . over lensTMStateFontDesc (`fromMaybe` maybeFontDesc)
      . over (lensTMStateConfig . lensOptions)
        ( set lensConfirmExit confirmExitVal
        . set lensShowMenu showMenuVal
        . set lensBoldIsBright boldIsBrightVal
        . set lensEnableSixel enableSixelVal
        . set lensAllowBold allowBoldVal
        . set lensWordCharExceptions wordCharExceptionsVal
        . over lensFontConfig (`fromMaybe` maybeFontConfig)
        . set lensScrollbackLen scrollbackLenVal
        . over lensShowScrollbar (`fromMaybe` maybeShowScrollbar)
        . over lensShowTabBar (`fromMaybe` maybeShowTabBar)
        . over lensCursorBlinkMode (`fromMaybe` maybeCursorBlinkMode)
        )

    -- Save the changes to the preferences files
    withMVar mvarTMState $ saveToPreferencesFile . view lensTMStateConfig

    -- Update the app with new settings
    applyNewPreferences mvarTMState

  widgetDestroy preferencesDialog
