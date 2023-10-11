-- | Module    : Termonad.Cli
-- Description : Termonad CLI argument parsing module.
-- Copyright   : (c) Dennis Gosnell, 2023
-- License     : BSD3
-- Stability   : experimental
-- Portability : POSIX
--
-- This module exposes Termonad's CLI argument parsing functionality.
--
-- The main function for parsing CLI arguments is 'parseCliArgs'.  The function
-- that knows how to combine CLI arguments with normal 'ConfigOptions' is
-- 'applyCliArgs'.

module Termonad.Cli where

import Termonad.Prelude

import Control.Applicative ((<|>), (<**>))
import Data.Text (pack)
import GI.Vte (CursorBlinkMode)
import Options.Applicative (fullDesc, info, helper, progDesc, ParserInfo, execParser, Parser, Mod, OptionFields, option, str, value, short, long, metavar, help, ReadM, maybeReader, auto, flag')
import Termonad.Types (ConfigOptions (..), Option (Set, Unset), ShowScrollbar, FontSize (..), ShowTabBar, showScrollbarFromString, showTabBarFromString, cursorBlinkModeFromString, FontConfig (..))


-- | A data type that contains arguments from the command line.
data CliArgs = CliArgs
  { cliConfigOptions :: CliConfigOptions
  , extraCliArgs :: ExtraCliArgs
  } deriving (Eq, Show)

-- | The default 'CliArgs'.  This corresponds to the value 'CliArgs' will
-- become when no CLI arguments have been passed.
--
-- >>> :{
--   let defCliArgs =
--         CliArgs
--           { cliConfigOptions = defaultCliConfigOptions
--           , extraCliArgs = defaultExtraCliArgs
--           }
--   in defaultCliArgs == defCliArgs
-- :}
-- True
defaultCliArgs :: CliArgs
defaultCliArgs =
  CliArgs
    { cliConfigOptions = defaultCliConfigOptions
    , extraCliArgs = defaultExtraCliArgs
    }

-- | CLI arguments that correspond to fields in 'ConfigOptions'.
--
-- See 'ConfigOptions' for what each of these options mean.
data CliConfigOptions = CliConfigOptions
  { cliConfFontFamily :: !(Option Text)
  , cliConfFontSize :: !(Option FontSize)
  , cliConfShowScrollbar :: !(Option ShowScrollbar)
  , cliConfScrollbackLen :: !(Option Integer)
  , cliConfConfirmExit :: !(Option Bool)
  , cliConfWordCharExceptions :: !(Option Text)
  , cliConfShowMenu :: !(Option Bool)
  , cliConfShowTabBar :: !(Option ShowTabBar)
  , cliConfCursorBlinkMode :: !(Option CursorBlinkMode)
  , cliConfBoldIsBright :: !(Option Bool)
  , cliConfEnableSixel :: !(Option Bool)
  , cliConfAllowBold :: !(Option Bool)
  } deriving (Eq, Show)

-- | The default 'CliConfigOptions'.  All 'Option's are 'Unset', which means
-- they won't override options from 'ConfigOptions' in 'applyCliArgs'.
--
-- >>> :{
--   let defCliConfOpt =
--         CliConfigOptions
--           { cliConfFontFamily = Unset
--           , cliConfFontSize = Unset
--           , cliConfShowScrollbar = Unset
--           , cliConfScrollbackLen = Unset
--           , cliConfConfirmExit = Unset
--           , cliConfWordCharExceptions = Unset
--           , cliConfShowMenu = Unset
--           , cliConfShowTabBar = Unset
--           , cliConfCursorBlinkMode = Unset
--           , cliConfBoldIsBright = Unset
--           , cliConfEnableSixel = Unset
--           , cliConfAllowBold = Unset
--           }
--   in defaultCliConfigOptions == defCliConfOpt
-- :}
-- True
defaultCliConfigOptions :: CliConfigOptions
defaultCliConfigOptions =
  CliConfigOptions
    { cliConfFontFamily = Unset
    , cliConfFontSize = Unset
    , cliConfShowScrollbar = Unset
    , cliConfScrollbackLen = Unset
    , cliConfConfirmExit = Unset
    , cliConfWordCharExceptions = Unset
    , cliConfShowMenu = Unset
    , cliConfShowTabBar = Unset
    , cliConfCursorBlinkMode = Unset
    , cliConfBoldIsBright = Unset
    , cliConfEnableSixel = Unset
    , cliConfAllowBold = Unset
    }

-- | Extra CLI arguments for values that don't make sense in 'ConfigOptions'.
data ExtraCliArgs = ExtraCliArgs
  deriving (Eq, Show)

-- | The default 'ExtraCliArgs'.
--
-- >>> :{
--   let defExtraCliArgs =
--         ExtraCliArgs
--   in defaultExtraCliArgs == defExtraCliArgs
-- :}
-- True
defaultExtraCliArgs :: ExtraCliArgs
defaultExtraCliArgs = ExtraCliArgs

-- | Similar to 'Options.Applicative.strOption', but specifically work on a
-- value that is an 'Option'.
strOption' :: IsString s => Mod OptionFields (Option s) -> Parser (Option s)
strOption' mods = option (fmap Set str) (value Unset <> mods)

-- | Similar to 'Options.Applicative.option', but specifically work on a
-- value that is an 'Option'.
option' :: ReadM a -> Mod OptionFields (Option a) -> Parser (Option a)
option' readM mods = option (fmap Set readM) (value Unset <> mods)

-- | Similar to 'Options.Applicative.maybeReader', but work on 'Text' instead
-- of 'String'.
maybeTextReader :: (Text -> Maybe a) -> ReadM a
maybeTextReader f = maybeReader (f . pack)

-- | Helper for making a 'flag' CLI argument that optionally takes a @no-@ prefix.
--
-- Example:
--
-- > 'optionFlag' 'True' 'False' 'f' 'n' "foo" "Does foo" "Does not do foo" :: Parser (Option Bool)
--
-- This creates a 'Parser' that accepts both a @--foo@ and a @--no-foo@ flag.
-- Passing @--foo@ returns @'Set' 'True'@, while passing @--no-foo@ returns
-- @'Set' 'False'@.  Passing neither @--foo@ nor @--no-foo@ returns 'Unset'.
--
-- TODO: This doesn't quite work.  If the user passes both @--foo@ and
-- @--no-foo@ flags, this should ideally take the value of the last flag
-- passed.  However, it appears that if you pass both flags, the second
-- flag is just not recognized and optparse-applicative raises an error.
optionFlag
  :: a -- ^ Value when specified /without/ @no-@ prefix.
  -> a -- ^ Value when specified /with/ @no-@ prefix.
  -> Char -- ^ Short flag for /without/ @no-@ prefix.
  -> Char -- ^ Short flag for /with/ @no-@ prefix.
  -> String -- ^ Long flag.
  -> String -- ^ Help text for /without/ @no-@ prefix option.
  -> String -- ^ Help text for /with/ @no-@ prefix option.
  -> Parser (Option a)
optionFlag valNormal valNo shortNormal shortNo longFlag helpNormal helpNo =
  flagNormal <|> flagNo <|> pure Unset
  where
    flagNormal =
      flag' (Set valNormal) (short shortNormal <> long longFlag <> help helpNormal)
    flagNo =
      flag' (Set valNo) (short shortNo <> long ("no-" <> longFlag) <> help helpNo)

cliConfigOptionsParser :: Parser CliConfigOptions
cliConfigOptionsParser =
  CliConfigOptions
    <$> fontFamilyParser
    <*> fontSizeParser
    <*> showScrollbarParser
    <*> scrollbackLenParser
    <*> confirmExitParser
    <*> wordCharExceptionsParser
    <*> showMenuParser
    <*> showTabBarParser
    <*> cursorBlinkModeParser
    <*> boldIsBrightParser
    <*> enableSixelParser
    <*> allowBoldParser

fontFamilyParser :: Parser (Option Text)
fontFamilyParser =
  strOption'
    ( short 'f' <>
      long "font-family" <>
      metavar "FONT_FAMILY" <>
      help
        "Font family to use. Defaults to \"Monospace\". Examples: \
        \\"DejaVu Sans Mono\", \"Source Code Pro\""
    )

fontSizeParser :: Parser (Option FontSize)
fontSizeParser = f <$> pointsParser <*> unitsParser
  where
    f :: Option Int -> Option Double -> Option FontSize
    f optionPoints optionUnits =
      case (fmap FontSizePoints optionPoints, fmap FontSizeUnits optionUnits) of
        (Unset, units) -> units
        (points, _) -> points

    pointsParser :: Parser (Option Int)
    pointsParser =
      option'
        auto
        ( short 'p' <>
          long "font-size-points" <>
          metavar "POINTS" <>
          help
            "Font size in POINTS. Defaults to \"12\" if not specified. \
            \If you specify both --font-size-points and --font-size-units, \
            \--font-size-points will take priority.  --font-size-points \
            \should be similar to font sizes you may be \
            \familiar with from other applications."
        )

    unitsParser :: Parser (Option Double)
    unitsParser =
      option'
        auto
        ( short 'u' <>
          long "font-size-units" <>
          metavar "UNITS" <>
          help
            "Font size in device units/pixels. Example: \"20.5\", \
            \\"30\".  If not specified, the default from \
            \--font-size-points is used."
        )

showScrollbarParser :: Parser (Option ShowScrollbar)
showScrollbarParser =
  option'
    (maybeTextReader showScrollbarFromString)
    ( short 'l' <>
      long "show-scrollbar" <>
      metavar "SHOW_SCROLLBAR" <>
      help
        "Whether or not to show a scrollbar in the terminal. \
        \Defaults to \"if-needed\".  Possible values = \"never\": \
        \never show the scrollbar, \"always\": always show the \
        \scrollbar, \"if-needed\": only show the scrollbar if \
        \enough text on the screen"
    )

scrollbackLenParser :: Parser (Option Integer)
scrollbackLenParser =
  option'
    auto
    ( short 'b' <>
      long "scrollback-length" <>
      metavar "SCROLLBACK_LENGTH" <>
      help
        "Number of lines to keep in the scrollback buffer in the \
        \terminal.  Defaults to 10000 lines.  Examples: \"200\", \
        \\"3000\""
    )

confirmExitParser :: Parser (Option Bool)
confirmExitParser =
  optionFlag
    True
    False
    'x'
    'C'
    "confirm-exit"
    "Ask for confirmation when closing terminals or Termonad itself. \
    \Defaults to asking for confirmation if not specified."
    "Do not ask for confirmation when closing terminals or Termonad \
    \itself. Defaults to asking for confirmation if not specified."

wordCharExceptionsParser :: Parser (Option Text)
wordCharExceptionsParser =
  strOption'
    ( short 'w' <>
      long "word-char-exceptions" <>
      metavar "EXCEPTIONS" <>
      help
        "The characters in this list will be counted as part of a word \
        \when double-clicking to select text in the terminal. Defaults \
        \to \"-#%&+,./=?@\\_~:\""
    )

showMenuParser :: Parser (Option Bool)
showMenuParser =
  optionFlag
    True
    False
    'm'
    'u'
    "show-menu"
    "Show the menu bar.  Defaults to showing the menu bar when not \
    \specified."
    "Do not show the menu bar.  Defaults to showing the menu bar when \
    \not specified."

showTabBarParser :: Parser (Option ShowTabBar)
showTabBarParser =
  option'
    (maybeTextReader showTabBarFromString)
    ( short 'l' <>
      long "show-tab-bar" <>
      metavar "SHOW_TAB_BAR" <>
      help
        "Whether or not to show the tab bar in the terminal. \
        \Defaults to \"if-needed\".  Possible values = \"never\": \
        \never show the tab bar, \"always\": always show the \
        \tab bar, \"if-needed\": only show the tab bar if \
        \multiple tabs are open."
    )

cursorBlinkModeParser :: Parser (Option CursorBlinkMode)
cursorBlinkModeParser =
  option'
    (maybeTextReader cursorBlinkModeFromString)
    ( short 'n' <>
      long "cursor-blink" <>
      metavar "BLINK_MODE" <>
      help
        "How to handle cursor blink.  Defaults to \"on\". Possible \
        \values = \"system\": follow system settings, \"on\": cursor \
        \blinks, \"off\": no cursor blink."
    )

boldIsBrightParser :: Parser (Option Bool)
boldIsBrightParser =
  optionFlag
    True
    False
    'd'
    'r'
    "bold-is-bright"
    "Force bold text to use bright colors.  Defaults to not forcing \
    \bold text to use bright colors."
    "Do not force bold text to use bright colors.  Defaults to not forcing \
    \bold text to use bright colors."

enableSixelParser :: Parser (Option Bool)
enableSixelParser =
  optionFlag
    True
    False
    's'
    'i'
    "sixel"
    "Enable SIXEL support.  Note that you need to build Termonad with \
    \a VTE with SIXEL support for this to work.  Defaults to not \
    \enabling SIXEL."
    "Disable SIXEL support.  Defaults to disabling SIXEL support."

allowBoldParser :: Parser (Option Bool)
allowBoldParser =
  optionFlag
    True
    False
    'a'
    'o'
    "allow-bold"
    "Allow Termonad to show bold text.  Defaults to enabled."
    "Disable Termonad from showing text as bold.  Defaults to \
    \allow showing text as bold."

extraCliArgsParser :: Parser ExtraCliArgs
extraCliArgsParser = pure ExtraCliArgs

cliArgsParser :: Parser CliArgs
cliArgsParser =
  CliArgs <$> cliConfigOptionsParser <*> extraCliArgsParser

cliArgsParserInfo :: ParserInfo CliArgs
cliArgsParserInfo =
  info
    (cliArgsParser <**> helper)
    ( fullDesc <>
      progDesc "A VTE-based terminal emulator configurable in Haskell"
    )

-- | Parse and return CliArguments.
parseCliArgs :: IO CliArgs
parseCliArgs = execParser cliArgsParserInfo

-- | Overwrite the arguments in 'ConfigOptions' that have been 'Set' in
-- 'CliArgs'.
--
-- >>> import Termonad.Types (defaultConfigOptions)
-- >>> let cliConfOpts = defaultCliConfigOptions { cliConfScrollbackLen = Set 50 }
-- >>> let cliArgs = defaultCliArgs { cliConfigOptions = cliConfOpts }
-- >>> let overwrittenConfOpts = defaultConfigOptions { scrollbackLen = 50 }
-- >>> applyCliArgs cliArgs defaultConfigOptions == overwrittenConfOpts
-- True
applyCliArgs :: CliArgs -> ConfigOptions -> ConfigOptions
applyCliArgs cliArgs confOpts =
  let oldFontConf = fontConfig confOpts
      newFontConfig =
        oldFontConf
          { fontFamily =
              fromOption
                (fontFamily oldFontConf)
                (cliConfFontFamily cliConfOpts)
          , fontSize =
              fromOption
                (fontSize oldFontConf)
                (cliConfFontSize cliConfOpts)
          }
  in
  confOpts
    { fontConfig = newFontConfig
    , showScrollbar = fromOpt showScrollbar cliConfShowScrollbar
    , scrollbackLen = fromOpt scrollbackLen cliConfScrollbackLen
    , confirmExit = fromOpt confirmExit cliConfConfirmExit
    , wordCharExceptions = fromOpt wordCharExceptions cliConfWordCharExceptions
    , showMenu = fromOpt showMenu cliConfShowMenu
    , showTabBar = fromOpt showTabBar cliConfShowTabBar
    , cursorBlinkMode = fromOpt cursorBlinkMode cliConfCursorBlinkMode
    , boldIsBright = fromOpt boldIsBright cliConfBoldIsBright
    , enableSixel = fromOpt enableSixel cliConfEnableSixel
    , allowBold = fromOpt allowBold cliConfAllowBold
    }
  where
    fromOpt
      :: forall a. (ConfigOptions -> a) -> (CliConfigOptions -> Option a) -> a
    fromOpt getConfVal getCliVal =
      fromOption
        (getConfVal confOpts)
        (getCliVal cliConfOpts)

    fromOption :: forall b. b -> Option b -> b
    fromOption defVal = \case
      Set a -> a
      Unset -> defVal

    cliConfOpts :: CliConfigOptions
    cliConfOpts = cliConfigOptions cliArgs
