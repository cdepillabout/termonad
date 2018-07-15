module Termonad.Term where

import Termonad.Prelude

import Control.Lens
import Data.GI.Base (new)
import GI.Gdk
  ( EventKey
  )
import GI.Gio
  ( noCancellable
  )
import GI.GLib
  ( SpawnFlags(SpawnFlagsDefault)
  )
import GI.Gtk
  ( Application
  , ApplicationWindow(ApplicationWindow)
  , Box(Box)
  , CssProvider(CssProvider)
  , Dialog(Dialog)
  , IconSize(IconSizeMenu)
  , IsWidget
  , MessageDialog(MessageDialog)
  , MessageType(MessageTypeQuestion)
  , Notebook(Notebook)
  , Orientation(OrientationHorizontal)
  , ResponseType(ResponseTypeNo, ResponseTypeYes)
  , ScrolledWindow(ScrolledWindow)
  , pattern STYLE_PROVIDER_PRIORITY_APPLICATION
  , applicationGetActiveWindow
  , applicationNew
  , applicationSetAccelsForAction
  , boxNew
  , builderNewFromString
  , builderSetApplication
  , buttonNewFromIconName
  , containerAdd
  , dialogAddButton
  , dialogGetContentArea
  , dialogNew
  , dialogRun
  , labelNew
  , noAdjustment
  , notebookAppendPage
  , notebookDetachTab
  , notebookSetCurrentPage
  , notebookSetTabLabelText
  , noWidget
  , onWidgetKeyPressEvent
  , scrolledWindowNew
  , setMessageDialogMessageType
  , setMessageDialogText
  , setWidgetHasFocus
  , setWidgetMargin
  , styleContextAddProviderForScreen
  , widgetDestroy
  , widgetGrabFocus
  , widgetShow
  , windowPresent
  , windowSetTransientFor
  )
import GI.Vte
  ( CursorBlinkMode(CursorBlinkModeOn)
  , PtyFlags(PtyFlagsDefault)
  , onTerminalChildExited
  , onTerminalWindowTitleChanged
  , terminalGetWindowTitle
  , terminalNew
  , terminalSetCursorBlinkMode
  , terminalSetFont
  , terminalSpawnSync
  )

import Termonad.FocusList
import Termonad.Gtk (objFromBuildUnsafe)
import Termonad.Types
import Termonad.XML (closeTabText)

import Text.Pretty.Simple

focusTerm :: Int -> TMState -> IO ()
focusTerm i mvarTMState = do
  note <- tmNotebook . tmStateNotebook <$> readMVar mvarTMState
  notebookSetCurrentPage note (fromIntegral i)

altNumSwitchTerm :: Int -> TMState -> IO ()
altNumSwitchTerm = focusTerm

termExitFocused :: TMState -> IO ()
termExitFocused mvarTMState = do
  tmState <- readMVar mvarTMState
  let maybeTab =
        tmState ^. lensTMStateNotebook . lensTMNotebookTabs . to getFLFocusItem
  case maybeTab of
    Nothing -> pure ()
    Just tab -> termExitWithConfirmation tab mvarTMState

termExitWithConfirmation :: TMNotebookTab -> TMState -> IO ()
termExitWithConfirmation tab mvarTMState = do
  tmState <- readMVar mvarTMState
  let app = tmState ^. lensTMStateApp
  win <- applicationGetActiveWindow app
  dialog <- dialogNew
  box <- dialogGetContentArea dialog
  label <- labelNew (Just "Close tab?")
  containerAdd box label
  widgetShow label
  setWidgetMargin label 10
  dialogAddButton dialog "No, do NOT close tab" (fromIntegral (fromEnum ResponseTypeNo))
  dialogAddButton dialog "Yes, close tab" (fromIntegral (fromEnum ResponseTypeYes))
  windowSetTransientFor dialog (Just win)
  res <- dialogRun dialog
  putStrLn $ "Result from running dialog: " <> tshow res
  widgetDestroy dialog
  case toEnum (fromIntegral res) of
    ResponseTypeYes -> termExit tab mvarTMState
    _ -> pure ()

termExit :: TMNotebookTab -> TMState -> IO ()
termExit tab mvarTMState = do
  detachTabAction <-
    modifyMVar mvarTMState $ \tmState -> do
      putStrLn "In termExit', tmState:"
      pPrint tmState
      let notebook = tmStateNotebook tmState
          detachTabAction =
            notebookDetachTab
              (tmNotebook notebook)
              (tmNotebookTabTermContainer tab)
      let newTabs = deleteFL tab (tmNotebookTabs notebook)
      let newTMState =
            set (lensTMStateNotebook . lensTMNotebookTabs) newTabs tmState
      putStrLn "In termExit', newTMState:"
      pPrint newTMState
      pure (newTMState, detachTabAction)
  detachTabAction

createScrolledWin :: IO ScrolledWindow
createScrolledWin = do
  scrolledWin <- scrolledWindowNew noAdjustment noAdjustment
  widgetShow scrolledWin
  pure scrolledWin

createNotebookTabLabel :: IO Box
createNotebookTabLabel = do
  box <- boxNew OrientationHorizontal 5
  label <- labelNew (Just "1. ")
  button <- buttonNewFromIconName (Just "window-close") (fromIntegral (fromEnum IconSizeMenu))
  containerAdd box label
  containerAdd box button
  widgetShow box
  widgetShow label
  widgetShow button
  pure box

createTerm :: (TMState -> EventKey -> IO Bool) -> TMState -> IO TMTerm
createTerm handleKeyPress mvarTMState = do
  putStrLn "createTerm, started..."
  withMVar mvarTMState (\tmState -> putStrLn $ "createTerm, started tmState: " <> tshow tmState)
  scrolledWin <- createScrolledWin
  TMState{tmStateFontDesc} <- readMVar mvarTMState
  vteTerm <- terminalNew
  terminalSetFont vteTerm (Just tmStateFontDesc)
  terminalSetCursorBlinkMode vteTerm CursorBlinkModeOn
  widgetShow vteTerm
  widgetGrabFocus $ vteTerm
  _termResVal <-
    terminalSpawnSync
      vteTerm
      [PtyFlagsDefault]
      Nothing
      ["/usr/bin/env", "bash"]
      Nothing
      ([SpawnFlagsDefault] :: [SpawnFlags])
      Nothing
      noCancellable
  tmTerm <- newTMTerm vteTerm
  let notebookTab = createTMNotebookTab scrolledWin tmTerm
  containerAdd scrolledWin vteTerm
  putStrLn "createTerm, created some stuff, about to go into mvar..."
  setCurrPageAction <-
    modifyMVar mvarTMState $ \tmState -> do
      putStrLn "createTerm, in mvar thing, started..."
      let notebook = tmStateNotebook tmState
          note = tmNotebook notebook
          tabs = tmNotebookTabs notebook
      putStrLn "createTerm, in mvar thing, about to notebookAppendPage..."
      tabLabel <- createNotebookTabLabel
      pageIndex <- notebookAppendPage note scrolledWin (Just tabLabel)
      let newTabs = appendFL tabs notebookTab
          newTMState =
            tmState & lensTMStateNotebook . lensTMNotebookTabs .~ newTabs
          setCurrPageAction = do
            notebookSetCurrentPage note pageIndex
      pure (newTMState, setCurrPageAction)
  putStrLn "createTerm, after mvar thing..."
  putStrLn "createTerm, in mvar thing, about to notebookSetCurrentPage..."
  setCurrPageAction
  void $ onTerminalWindowTitleChanged vteTerm $ do
    title <- terminalGetWindowTitle vteTerm
    TMState{tmStateNotebook} <- readMVar mvarTMState
    let notebook = tmNotebook tmStateNotebook
    -- notebookSetTabLabelText notebook scrolledWin title
    pure ()
  void $ onWidgetKeyPressEvent vteTerm $ handleKeyPress mvarTMState
  void $ onWidgetKeyPressEvent scrolledWin $ handleKeyPress mvarTMState
  void $ onTerminalChildExited vteTerm $ \_ -> termExit notebookTab mvarTMState
  withMVar mvarTMState (\tmState -> putStrLn $ "createTerm, ending tmState: " <> tshow tmState)
  pure tmTerm
