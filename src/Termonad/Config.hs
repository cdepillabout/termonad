{-# LANGUAGE TemplateHaskell #-}

module Termonad.Config where

import Termonad.Prelude hiding ((\\), index)

import Control.Lens (makeLensesFor, makePrisms)
import Data.Colour (Colour, affineCombo)
import Data.Colour.Names (black)
import Data.Colour.SRGB (sRGB24)
import qualified Data.Foldable
import GI.Vte (CursorBlinkMode(CursorBlinkModeOn))

import Termonad.Config.Vec
  (I(I), M, N3, N24, N6, N8, Prod((:<), Ø), Vec, VecT((:*), ØV), fin, mgen_, vgen_)

-----------------
-- Font Config --
-----------------

-- | The font size for the Termonad terminal.  There are two ways to set the
-- fontsize, corresponding to the two different ways to set the font size in
-- the Pango font rendering library.
--
-- If you're not sure which to use, try 'FontSizePoints' first and see how it
-- looks.  It should generally correspond to font sizes you are used to from
-- other applications.
data FontSize
  = FontSizePoints Int
    -- ^ This sets the font size based on \"points\".  The conversion between a
    -- point and an actual size depends on the system configuration and the
    -- output device.  The function 'GI.Pango.fontDescriptionSetSize' is used
    -- to set the font size.  See the documentation for that function for more
    -- info.
  | FontSizeUnits Double
    -- ^ This sets the font size based on \"device units\".  In general, this
    -- can be thought of as one pixel.  The function
    -- 'GI.Pango.fontDescriptionSetAbsoluteSize' is used to set the font size.
    -- See the documentation for that function for more info.
  deriving (Eq, Show)

-- | The default 'FontSize' used if not specified.
--
-- >>> defaultFontSize
-- FontSizePoints 12
defaultFontSize :: FontSize
defaultFontSize = FontSizePoints 12

$(makePrisms ''FontSize)

-- | Settings for the font to be used in Termonad.
data FontConfig = FontConfig
  { fontFamily :: !Text
    -- ^ The font family to use.  Example: @"DejaVu Sans Mono"@ or @"Source Code Pro"@
  , fontSize :: !FontSize
    -- ^ The font size.
  } deriving (Eq, Show)

-- | The default 'FontConfig' to use if not specified.
--
-- >>> defaultFontConfig == FontConfig {fontFamily = "Monospace", fontSize = defaultFontSize}
-- True
defaultFontConfig :: FontConfig
defaultFontConfig =
  FontConfig
    { fontFamily = "Monospace"
    , fontSize = defaultFontSize
    }

$(makeLensesFor
    [ ("fontFamily", "lensFontFamily")
    , ("fontSize", "lensFontSize")
    ]
    ''FontConfig
 )

-------------------
-- Colour Config --
-------------------


-- | This is the color palette to use for the terminal. Each data constructor
-- lets you set progressively more colors.  These colors are used in
-- <https://en.wikipedia.org/wiki/ANSI_escape_code#Colors ANSI escape codes>.
--
-- There are 256 total terminal colors. 'BasicPalette' lets you set the first 8,
-- 'ExtendedPalette' lets you set the first 16, 'ColourCubePalette' lets you set
-- the first 232, and 'FullPalette' lets you set all 256.
--
-- The first 8 colors codes are the standard colors. The next 8 are the
-- high-intensity colors. The next 216 are a full color cube. The last 24 are a
-- grey scale.
--
-- The following image gives an idea of what each individual color looks like:
--
-- <<https://raw.githubusercontent.com/cdepillabout/termonad/master/img/terminal-colors.png>>
--
-- This picture does not exactly match up with Termonad's default colors, but it gives an
-- idea of what each block of colors represents.
--
-- You can use 'defaultStandardColours', 'defaultLightColours',
-- 'defaultColourCube', and 'defaultGreyscale' as a starting point to
-- customize the colors. The only time you'd need to use a constructor other
-- than 'NoPalette' is when you want to customize the default colors.
data Palette c
  = NoPalette
  -- ^ Don't set any colors and just use the default from VTE.  This is a black
  -- background with light grey text.
  | BasicPalette !(Vec N8 c)
  -- ^ Set the colors from the standard colors.
  | ExtendedPalette !(Vec N8 c) !(Vec N8 c)
  -- ^ Set the colors from the extended (light) colors (as well as standard colors).
  | ColourCubePalette !(Vec N8 c) !(Vec N8 c) !(M '[N6, N6, N6] c)
  -- ^ Set the colors from the color cube (as well as the standard colors and
  -- extended colors).
  | FullPalette !(Vec N8 c) !(Vec N8 c) !(M '[N6, N6, N6] c) !(Vec N24 c)
  -- ^ Set the colors from the grey scale (as well as the standard colors,
  -- extended colors, and color cube).
  deriving (Eq, Show, Functor, Foldable)

-- | Convert a 'Palette' to a list of colors.  This is helpful for debugging.
paletteToList :: Palette c -> [c]
paletteToList = Data.Foldable.toList

coloursFromBits :: (Ord b, Floating b) => Word8 -> Word8 -> Vec N8 (Colour b)
coloursFromBits scale offset = vgen_ $ I . (sRGB24 <$> cmp 0 <*> cmp 1 <*> cmp 2)
  where
    bit :: Int -> Int -> Int
    bit m i = i `div` (2 ^ m) `mod` 2
    cmp i = (offset +) . (scale *) . fromIntegral . bit i . fin

defaultStandardColours :: (Ord b, Floating b) => Vec N8 (Colour b)
defaultStandardColours = coloursFromBits 192 0

defaultLightColours :: (Ord b, Floating b) => Vec N8 (Colour b)
defaultLightColours = coloursFromBits 192 63

-- | Specify a colour cube with one colour vector for its displacement and three
-- colour vectors for its edges. Produces a uniform 6x6x6 grid bounded by
-- and orthognal to the faces.
cube
  :: Fractional b => Colour b -> Vec N3 (Colour b) -> M [N6, N6, N6] (Colour b)
cube d (I i :* I j :* I k :* ØV) = mgen_ $ \(x :< y :< z :< Ø) ->
  affineCombo [(1, d), (coef x, i), (coef y, j), (coef z, k)] black
  where coef n = fromIntegral (fin n) / 5

defaultColourCube :: (Ord b, Floating b) => M [N6, N6, N6] (Colour b)
defaultColourCube = mgen_ $ \(x :< y :< z :< Ø) -> sRGB24 (cmp x) (cmp y) (cmp z)
  where
    cmp i =
      let i' = fromIntegral (fin i)
      in signum i' * 55 + 40 * i'

defaultGreyscale :: (Ord b, Floating b) => Vec N24 (Colour b)
defaultGreyscale = vgen_ $ \n -> I $
  let l = 8 + 10 * fromIntegral (fin n)
  in sRGB24 l l l

-- | This data type represents an option that can either be 'Set' or 'Unset'.
--
-- This data type is used in situations where leaving an option unset results
-- in a special state that is not representable by setting any specific value.
--
-- An example of this is 'cursorFgColour' and 'cursorBgColour'.  By default,
-- 'cursorFgColour' and 'cursorBgColour' are both 'Unset'.  However, when
-- 'cursorBgColour' is 'Set', 'cursorFgColour' defaults to the color of the text
-- underneath.  There is no way to represent this by 'Set'ting 'cursorFgColour'.
data Option a = Unset | Set !a
  deriving (Show, Read, Eq, Ord, Functor, Foldable)

whenSet :: Monoid m => Option a -> (a -> m) -> m
whenSet = \case
  Unset -> \_ -> mempty
  Set x -> \f -> f x

-- | NB: Currently due to issues either with VTE or the bindings generated for
-- Haskell, background colour cannot be set independently of the palette.
-- The @backgroundColour@ field will be ignored and the 0th colour in the
-- palette (usually black) will be used as the background colour.
--
-- Termonad will behave differently depending on the combination
-- 'cursorFgColour' and 'cursorBgColour' being 'Set' vs. 'Unset'.
-- Here is the summary of the different possibilities:
--
-- * 'cursorFgColour' is 'Set' and 'cursorBgColour' is 'Set'
--
--   The foreground and background colors of the cursor are as you have set.
--
-- * 'cursorFgColour' is 'Set' and 'cursorBgColour' is 'Unset'
--
--   The cursor background color turns completely black so that it is not
--   visible.  The foreground color of the cursor is the color that you have
--   'Set'.  This ends up being mostly unusable, so you are recommended to
--   always 'Set' 'cursorBgColour' when you have 'Set' 'cursorFgColour'.
--
-- * 'cursorFgColour' is 'Unset' and 'cursorBgColour' is 'Set'
--
--   The cursor background color becomes the color you 'Set', while the cursor
--   foreground color doesn't change from the letter it is over.  For instance,
--   imagine there is a letter on the screen with a black background and a
--   green foreground.  If you bring the cursor overtop of it, the cursor
--   background will be the color you have 'Set', while the cursor foreground
--   will be green.
--
--   This is completely usable, but is slightly annoying if you place the
--   cursor over a letter with the same foreground color as the cursor because
--   the letter will not be readable.  For instance, imagine you have set your
--   cursor background color to red, and somewhere on the screen there is a
--   letter with a black background and a red foreground.  If you move your
--   cursor over the letter, the background of the cursor will be red (as you
--   have set), and the cursor foreground will be red (to match the original
--   foreground color of the letter), but this will make it so you can't
--   actually read the letter, because the foreground and background are both
--   red.
--
-- * 'cursorFgColour' is 'Unset' and 'cursorBgColour' is 'Unset'
--
--   This combination makes the cursor inverse of whatever text it is over.
--   If your cursor is over red text with a black background, the cursor
--   background will be red and the cursor foreground will be black.
--
--   This is the default.
data ColourConfig c = ColourConfig
  { cursorFgColour :: !(Option c)
  , cursorBgColour :: !(Option c)
  , foregroundColour :: !c
  , backgroundColour :: !c
  , palette :: !(Palette c)
  } deriving (Eq, Show, Functor)

$(makeLensesFor
    [ ("cursorFgColour", "lensCursorFgColour")
    , ("cursorBgColour", "lensCursorBgColour")
    , ("foregroundColour", "lensForegroundColour")
    , ("backgroundColour", "lensBackgroundColour")
    , ("palette", "lensPalette")
    ]
    ''ColourConfig
 )

defaultColourConfig :: (Ord b, Floating b) => ColourConfig (Colour b)
defaultColourConfig = ColourConfig
  { cursorFgColour = Unset
  , cursorBgColour = Unset
  , foregroundColour = sRGB24 192 192 192
  , backgroundColour = black
  , palette = NoPalette
  }

---------------
-- Scrollbar --
---------------

data ShowScrollbar
  = ShowScrollbarNever
  | ShowScrollbarAlways
  | ShowScrollbarIfNeeded
  deriving (Eq, Show)

------------
-- Tabbar --
------------

data ShowTabBar
  = ShowTabBarNever
  | ShowTabBarAlways
  | ShowTabBarIfNeeded
  deriving (Eq, Show)

--------------
-- TMConfig --
--------------

data TMConfig = TMConfig
  { fontConfig :: !FontConfig
  , showScrollbar :: !ShowScrollbar
  , colourConfig :: !(ColourConfig (Colour Double))
  , scrollbackLen :: !Integer
  , confirmExit :: !Bool
  , wordCharExceptions :: !Text
  , showMenu :: !Bool
  , showTabBar :: !ShowTabBar
  , cursorBlinkMode :: !CursorBlinkMode
  } deriving (Eq, Show)

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
    ]
    ''TMConfig
 )

defaultTMConfig :: TMConfig
defaultTMConfig =
  TMConfig
    { fontConfig = defaultFontConfig
    , showScrollbar = ShowScrollbarIfNeeded
    , colourConfig = defaultColourConfig
    , scrollbackLen = 10000
    , confirmExit = True
    , wordCharExceptions = "-#%&+,./=?@\\_~\183:"
    , showMenu = True
    , showTabBar = ShowTabBarIfNeeded
    , cursorBlinkMode = CursorBlinkModeOn
    }
