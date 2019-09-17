{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Module    : Termonad.Config.Colour
-- Description : Termonad Configuration Colour Options
-- Copyright   : (c) Dennis Gosnell, 2018
-- License     : BSD3
-- Stability   : experimental
-- Portability : POSIX
--
-- To use this config extension in your @~\/.config\/termonad\/termonad.hs@, first
-- import this module. Create a new 'ColourExtension' with the 'createColourExtension' function.
-- Then add the 'ColourExtension' to your 'TMConfig' with the 'addColourExtension' function.
--
-- See
-- <https://github.com/cdepillabout/termonad/blob/master/example-config/ExampleColourExtension.hs this code>
-- for a simple example.
--
-- When setting colors, you may find it convenient to use the
-- <http://hackage.haskell.org/package/print-console-colors print-console-colors>
-- package, which provides an executable called @print-console-colors@ that prints
-- all of the colors for your terminal.

module Termonad.Config.Colour
  ( -- * Colour Config
      ColourConfig(..)
    , defaultColourConfig
    -- ** Colour Config Lenses
    , lensCursorFgColour
    , lensCursorBgColour
    , lensForegroundColour
    , lensBackgroundColour
    , lensPalette
    -- * Colour Extension
    , ColourExtension(..)
    , createColourExtension
    , createDefColourExtension
    , addColourExtension
    , addColourConfig
    , colourHook
    , addColourHook
    -- * Palette
    , Palette(..)
    , defaultStandardColours
    , defaultLightColours
    , defaultColourCube
    , defaultGreyscale
    -- * Colour
    -- | Check out the "Data.Colour" module for more info about 'AlphaColour'.
    , AlphaColour
    , createColour
    , sRGB32
    , sRGB32show
    , opaque
    , transparent
    -- * Debugging and Internal Methods
    , showColourVec
    , showColourCube
    , paletteToList
    , coloursFromBits
    , cube
    -- * Doctest setup
    -- $setup
  ) where

import Termonad.Prelude hiding ((\\), index)

import Control.Lens ((%~), makeLensesFor)
import Data.Colour
  ( AlphaColour
  , Colour
  , affineCombo
  , alphaChannel
  , black
  , darken
  , opaque
  , over
  , transparent
  , withOpacity
  )
import Data.Colour.SRGB (RGB(RGB), toSRGB, toSRGB24, sRGB24)
import qualified Data.Foldable
import GI.Gdk
  ( RGBA
  , newZeroRGBA
  , setRGBAAlpha
  , setRGBABlue
  , setRGBAGreen
  , setRGBARed
  )
import GI.Vte
  ( Terminal
  , terminalSetColors
  , terminalSetColorCursor
#ifdef VTE_VERSION_GEQ_0_44
  , terminalSetColorCursorForeground
#endif
  , terminalSetColorBackground
  , terminalSetColorForeground
  )
import Text.Printf (printf)
import Text.Show (showString)

import Termonad.Config.Vec
import Termonad.Lenses (lensCreateTermHook, lensHooks)
import Termonad.Types
  ( Option(Unset)
  , TMConfig
  , TMState
  , whenSet
  )

-- $setup
-- >>> import Data.Colour.Names (green, red)
-- >>> import Data.Colour.SRGB (sRGB24show)

-------------------
-- Colour Config --
-------------------

-- | This is the color palette to use for the terminal. Each data constructor
-- lets you set progressively more colors.  These colors are used by the
-- terminal to render
-- <https://en.wikipedia.org/wiki/ANSI_escape_code#Colors ANSI escape color codes>.
--
-- There are 256 total terminal colors. 'BasicPalette' lets you set the first 8,
-- 'ExtendedPalette' lets you set the first 16, 'ColourCubePalette' lets you set
-- the first 232, and 'FullPalette' lets you set all 256.
--
-- The first 8 colors codes are the standard colors. The next 8 are the
-- extended (light) colors. The next 216 are a full color cube. The last 24 are a
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
-- That is to say, using 'FullPalette' with all the defaults should give you the
-- same result as using 'NoPalette'.
data Palette c
  = NoPalette
  -- ^ Don't set any colors and just use the default from VTE.  This is a black
  -- background with light grey text.
  | BasicPalette !(Vec N8 c)
  -- ^ Set the colors from the standard colors.
  | ExtendedPalette !(Vec N8 c) !(Vec N8 c)
  -- ^ Set the colors from the extended (light) colors (as well as standard colors).
  | ColourCubePalette !(Vec N8 c) !(Vec N8 c) !(Matrix '[N6, N6, N6] c)
  -- ^ Set the colors from the color cube (as well as the standard colors and
  -- extended colors).
  | FullPalette !(Vec N8 c) !(Vec N8 c) !(Matrix '[N6, N6, N6] c) !(Vec N24 c)
  -- ^ Set the colors from the grey scale (as well as the standard colors,
  -- extended colors, and color cube).
  deriving (Eq, Show, Functor, Foldable)

-- | Convert a 'Palette' to a list of colors.  This is helpful for debugging.
paletteToList :: Palette c -> [c]
paletteToList = Data.Foldable.toList

-- | Create a vector of colors based on input bits.
--
-- This is used to derive 'defaultStandardColours' and 'defaultLightColours'.
--
-- >>> coloursFromBits 192 0 == defaultStandardColours
-- True
--
-- >>> coloursFromBits 192 63 == defaultLightColours
-- True
--
-- In general, as an end-user, you shouldn't need to use this.
coloursFromBits :: forall b. (Ord b, Floating b) => Word8 -> Word8 -> Vec N8 (AlphaColour b)
coloursFromBits scale offset = genVec_ createElem
  where
    createElem :: Fin N8 -> AlphaColour b
    createElem finN =
      let red = cmp 0 finN
          green = cmp 1 finN
          blue = cmp 2 finN
          color = opaque $ sRGB24 red green blue
      in color

    cmp :: Int -> Fin N8 -> Word8
    cmp i = (offset +) . (scale *) . fromIntegral . bit i . toIntFin

    bit :: Int -> Int -> Int
    bit m i = i `div` (2 ^ m) `mod` 2

-- | A 'Vec' of standard colors.  Default value for 'BasicPalette'.
--
-- >>> showColourVec defaultStandardColours
-- ["#000000ff","#c00000ff","#00c000ff","#c0c000ff","#0000c0ff","#c000c0ff","#00c0c0ff","#c0c0c0ff"]
defaultStandardColours :: (Ord b, Floating b) => Vec N8 (AlphaColour b)
defaultStandardColours = coloursFromBits 192 0

-- | A 'Vec' of extended (light) colors.  Default value for 'ExtendedPalette'.
--
-- >>> showColourVec defaultLightColours
-- ["#3f3f3fff","#ff3f3fff","#3fff3fff","#ffff3fff","#3f3fffff","#ff3fffff","#3fffffff","#ffffffff"]
defaultLightColours :: (Ord b, Floating b) => Vec N8 (AlphaColour b)
defaultLightColours = coloursFromBits 192 63

-- | Convert an 'AlphaColour' to a 'Colour'.
--
-- >>> sRGB24show $ pureColour (opaque green)
-- "#008000"
-- >>> sRGB24show $ pureColour (sRGB32 0x30 0x40 0x50 0x80)
-- "#304050"
--
-- We assume that black is the pure color for a fully transparent
-- 'AlphaColour'.
--
-- >>> sRGB24show $ pureColour transparent
-- "#000000"
--
-- This function has been taken from:
-- https://wiki.haskell.org/Colour#Getting_semi-transparent_coordinates
pureColour :: AlphaColour Double -> Colour Double
pureColour alaphaColour
  | a > 0 = darken (recip a) (alaphaColour `over` black)
  | otherwise = black
  where
    a :: Double
    a = alphaChannel alaphaColour

-- | 'round's and then clamps the input between 0 and 'maxBound'.
--
-- Rounds the input:
--
-- >>> quantize (100.2 :: Double) :: Word8
-- 100
--
-- Clamps to 'minBound' if input is too low:
--
-- >>> quantize (-3 :: Double) :: Word8
-- 0
--
-- Clamps to 'maxBound' if input is too high:
-- >>> quantize (1000 :: Double) :: Word8
-- 255
--
-- Function used to quantize the alpha channel in the same way as the 'RGB'
-- components. It has been copied from "Data.Colour.Internal".
quantize :: forall a b. (RealFrac a, Integral b, Bounded b) => a -> b
quantize x
  | x <= fromIntegral l = l
  | fromIntegral h <= x = h
  | otherwise           = round x
  where
    l :: b
    l = minBound

    h :: b
    h = maxBound

-- | Show an 'AlphaColour' in hex.
--
-- >>> sRGB32show (opaque red)
-- "#ff0000ff"
--
-- Similar to 'Data.Colour.SRGB.sRGB24show'.
sRGB32show :: AlphaColour Double -> String
sRGB32show c = printf "#%02x%02x%02x%02x" r g b a
  where
    r, g, b :: Word8
    RGB r g b = toSRGB24 $ pureColour c

    -- This about the same code as in Data.Colour.SRGB.toSRGBBounded
    a :: Word8
    a = quantize (255 * alphaChannel c)

-- | Create an 'AlphaColour' from a four 'Word8's.
--
-- >>> sRGB32show $ sRGB32 64 96 128 255
-- "#406080ff"
-- >>> sRGB32show $ sRGB32 0x08 0x10 0x20 0x01
-- "#08102001"
--
-- Note that if you specify the alpha as 0 (which means completely
-- translucent), all the color channels will be set to 0 as well.
--
-- >>> sRGB32show $ sRGB32 100 150 200 0
-- "#00000000"
--
-- Similar to 'sRGB24' but also includes an alpha channel.  Most users will
-- probably want to use 'createColour' instead.
sRGB32
  :: Word8 -- ^ red channel
  -> Word8 -- ^ green channel
  -> Word8 -- ^ blue channel
  -> Word8 -- ^ alpha channel
  -> AlphaColour Double
sRGB32 r g b 255 = withOpacity (sRGB24 r g b) 1
sRGB32 r g b a =
  let aDouble = fromIntegral a / 255
  in (withOpacity (sRGB24 r g b) aDouble)

-- | Create an 'AlphaColour' that is fully 'opaque'.
--
-- >>> sRGB32show $ createColour 64 96 128
-- "#406080ff"
-- >>> sRGB32show $ createColour 0 0 0
-- "#000000ff"
--
-- Similar to 'sRGB24' but for 'AlphaColour'.
createColour
  :: Word8 -- ^ red channel
  -> Word8 -- ^ green channel
  -> Word8 -- ^ blue channel
  -> AlphaColour Double
createColour r g b = sRGB32 r g b 255

-- | A helper function for showing all the colors in 'Vec' of colors.
showColourVec :: forall n. Vec n (AlphaColour Double) -> [String]
showColourVec = fmap sRGB32show . Data.Foldable.toList

-- | Specify a colour cube with one colour vector for its displacement and three
-- colour vectors for its edges. Produces a uniform 6x6x6 grid bounded by
-- and orthognal to the faces.
cube ::
     forall b. Fractional b
  => AlphaColour b
  -> Vec N3 (AlphaColour b)
  -> Matrix '[ N6, N6, N6] (AlphaColour b)
cube d (i :* j :* k :* EmptyVec) =
  genMatrix_ $
    \(x :< y :< z :< EmptyHList) ->
      affineCombo [(1, d), (coef x, i), (coef y, j), (coef z, k)] $ opaque black
  where
    coef :: Fin N6 -> b
    coef fin' = fromIntegral (toIntFin fin') / 5

-- | A matrix of a 6 x 6 x 6 color cube. Default value for 'ColourCubePalette'.
--
-- >>> putStrLn $ pack $ showColourCube defaultColourCube
-- [ [ #000000ff, #00005fff, #000087ff, #0000afff, #0000d7ff, #0000ffff
--   , #005f00ff, #005f5fff, #005f87ff, #005fafff, #005fd7ff, #005fffff
--   , #008700ff, #00875fff, #008787ff, #0087afff, #0087d7ff, #0087ffff
--   , #00af00ff, #00af5fff, #00af87ff, #00afafff, #00afd7ff, #00afffff
--   , #00d700ff, #00d75fff, #00d787ff, #00d7afff, #00d7d7ff, #00d7ffff
--   , #00ff00ff, #00ff5fff, #00ff87ff, #00ffafff, #00ffd7ff, #00ffffff
--   ]
-- , [ #5f0000ff, #5f005fff, #5f0087ff, #5f00afff, #5f00d7ff, #5f00ffff
--   , #5f5f00ff, #5f5f5fff, #5f5f87ff, #5f5fafff, #5f5fd7ff, #5f5fffff
--   , #5f8700ff, #5f875fff, #5f8787ff, #5f87afff, #5f87d7ff, #5f87ffff
--   , #5faf00ff, #5faf5fff, #5faf87ff, #5fafafff, #5fafd7ff, #5fafffff
--   , #5fd700ff, #5fd75fff, #5fd787ff, #5fd7afff, #5fd7d7ff, #5fd7ffff
--   , #5fff00ff, #5fff5fff, #5fff87ff, #5fffafff, #5fffd7ff, #5fffffff
--   ]
-- , [ #870000ff, #87005fff, #870087ff, #8700afff, #8700d7ff, #8700ffff
--   , #875f00ff, #875f5fff, #875f87ff, #875fafff, #875fd7ff, #875fffff
--   , #878700ff, #87875fff, #878787ff, #8787afff, #8787d7ff, #8787ffff
--   , #87af00ff, #87af5fff, #87af87ff, #87afafff, #87afd7ff, #87afffff
--   , #87d700ff, #87d75fff, #87d787ff, #87d7afff, #87d7d7ff, #87d7ffff
--   , #87ff00ff, #87ff5fff, #87ff87ff, #87ffafff, #87ffd7ff, #87ffffff
--   ]
-- , [ #af0000ff, #af005fff, #af0087ff, #af00afff, #af00d7ff, #af00ffff
--   , #af5f00ff, #af5f5fff, #af5f87ff, #af5fafff, #af5fd7ff, #af5fffff
--   , #af8700ff, #af875fff, #af8787ff, #af87afff, #af87d7ff, #af87ffff
--   , #afaf00ff, #afaf5fff, #afaf87ff, #afafafff, #afafd7ff, #afafffff
--   , #afd700ff, #afd75fff, #afd787ff, #afd7afff, #afd7d7ff, #afd7ffff
--   , #afff00ff, #afff5fff, #afff87ff, #afffafff, #afffd7ff, #afffffff
--   ]
-- , [ #d70000ff, #d7005fff, #d70087ff, #d700afff, #d700d7ff, #d700ffff
--   , #d75f00ff, #d75f5fff, #d75f87ff, #d75fafff, #d75fd7ff, #d75fffff
--   , #d78700ff, #d7875fff, #d78787ff, #d787afff, #d787d7ff, #d787ffff
--   , #d7af00ff, #d7af5fff, #d7af87ff, #d7afafff, #d7afd7ff, #d7afffff
--   , #d7d700ff, #d7d75fff, #d7d787ff, #d7d7afff, #d7d7d7ff, #d7d7ffff
--   , #d7ff00ff, #d7ff5fff, #d7ff87ff, #d7ffafff, #d7ffd7ff, #d7ffffff
--   ]
-- , [ #ff0000ff, #ff005fff, #ff0087ff, #ff00afff, #ff00d7ff, #ff00ffff
--   , #ff5f00ff, #ff5f5fff, #ff5f87ff, #ff5fafff, #ff5fd7ff, #ff5fffff
--   , #ff8700ff, #ff875fff, #ff8787ff, #ff87afff, #ff87d7ff, #ff87ffff
--   , #ffaf00ff, #ffaf5fff, #ffaf87ff, #ffafafff, #ffafd7ff, #ffafffff
--   , #ffd700ff, #ffd75fff, #ffd787ff, #ffd7afff, #ffd7d7ff, #ffd7ffff
--   , #ffff00ff, #ffff5fff, #ffff87ff, #ffffafff, #ffffd7ff, #ffffffff
--   ]
-- ]
defaultColourCube :: (Ord b, Floating b) => Matrix '[N6, N6, N6] (AlphaColour b)
defaultColourCube =
  genMatrix_ $ \(x :< y :< z :< EmptyHList) -> opaque $ sRGB24 (cmp x) (cmp y) (cmp z)
  where
    cmp :: Fin N6 -> Word8
    cmp i =
      let i' = fromIntegral (toIntFin i)
      in signum i' * 55 + 40 * i'

-- | Helper function for showing all the colors in a color cube. This is used
-- for debugging.
showColourCube :: Matrix '[N6, N6, N6] (AlphaColour Double) -> String
showColourCube matrix =
  -- TODO: This function will only work with a 6x6x6 matrix, but it could be
  -- generalized to work with any Rank-3 matrix.
  let itemList = Data.Foldable.toList matrix
  in showSColourCube itemList ""
  where
    showSColourCube :: [AlphaColour Double] -> String -> String
    showSColourCube itemList =
      showString "[ " .
      showSquare 0 itemList .
      showString ", " .
      showSquare 1 itemList .
      showString ", " .
      showSquare 2 itemList .
      showString ", " .
      showSquare 3 itemList .
      showString ", " .
      showSquare 4 itemList .
      showString ", " .
      showSquare 5 itemList .
      showString "]"

    showSquare :: Int -> [AlphaColour Double] -> String -> String
    showSquare i colours =
      showString "[ " .
      showRow i 0 colours .
      showString ", " .
      showRow i 1 colours .
      showString ", " .
      showRow i 2 colours .
      showString ", " .
      showRow i 3 colours .
      showString ", " .
      showRow i 4 colours .
      showString ", " .
      showRow i 5 colours .
      showString "]\n"

    showRow :: Int -> Int -> [AlphaColour Double] -> String -> String
    showRow i j colours =
      showCol (headEx $ drop (i * 36 + j * 6 + 0) colours) .
      showString ", " .
      showCol (headEx $ drop (i * 36 + j * 6 + 1) colours) .
      showString ", " .
      showCol (headEx $ drop (i * 36 + j * 6 + 2) colours) .
      showString ", " .
      showCol (headEx $ drop (i * 36 + j * 6 + 3) colours) .
      showString ", " .
      showCol (headEx $ drop (i * 36 + j * 6 + 4) colours) .
      showString ", " .
      showCol (headEx $ drop (i * 36 + j * 6 + 5) colours) .
      showString "\n  "

    showCol :: AlphaColour Double -> String -> String
    showCol col str = sRGB32show col <> str

-- | A 'Vec' of a grey scale.  Default value for 'FullPalette'.
--
-- >>> showColourVec defaultGreyscale
-- ["#080808ff","#121212ff","#1c1c1cff","#262626ff","#303030ff","#3a3a3aff","#444444ff","#4e4e4eff","#585858ff","#626262ff","#6c6c6cff","#767676ff","#808080ff","#8a8a8aff","#949494ff","#9e9e9eff","#a8a8a8ff","#b2b2b2ff","#bcbcbcff","#c6c6c6ff","#d0d0d0ff","#dadadaff","#e4e4e4ff","#eeeeeeff"]
defaultGreyscale :: (Ord b, Floating b) => Vec N24 (AlphaColour b)
defaultGreyscale = genVec_ $ \n ->
  let l = 8 + 10 * fromIntegral (toIntFin n)
  in opaque $ sRGB24 l l l

-- | The configuration for the colors used by Termonad.
--
-- 'foregroundColour' and 'backgroundColour' allow you to set the color of the
-- foreground text and background of the terminal.
--
-- 'palette' allows you to set the full color palette used by the terminal.
-- See 'Palette' for more information.
--
-- If you don't set 'foregroundColour', 'backgroundColour', or 'palette', the
-- defaults from VTE are used.
--
-- If you want to use a terminal with a white (or light) background and a black
-- foreground, it may be a good idea to change some of the colors in the
-- 'Palette' as well.
--
-- VTE works as follows: if you don't explicitly set a background or foreground color,
-- it takes the 0th colour from the 'palette' to be the background color, and the 7th
-- colour from the 'palette' to be the foreground color.  If you notice oddities with
-- colouring in certain applications, it may be helpful to make sure that these
-- 'palette' colours match up with the 'backgroundColour' and 'foregroundColour' you
-- have set.)
--
-- 'cursorFgColour' and 'cursorBgColour' allow you to set the foreground color
-- of the text under the cursor, as well as the color of the cursor itself.
--
-- Termonad will behave differently depending on the combination
-- 'cursorFgColour' and 'cursorBgColour' being 'Set' vs. 'Unset'.
-- Here is the summary of the different possibilities:
--
-- * 'cursorFgColour' is 'Set' and 'cursorBgColour' is 'Set'
--
--     The foreground and background colors of the cursor are as you have set.
--
-- * 'cursorFgColour' is 'Set' and 'cursorBgColour' is 'Unset'
--
--     The cursor background color turns completely black so that it is not
--     visible.  The foreground color of the cursor is the color that you have
--     'Set'.  This ends up being mostly unusable, so you are recommended to
--     always 'Set' 'cursorBgColour' when you have 'Set' 'cursorFgColour'.
--
-- * 'cursorFgColour' is 'Unset' and 'cursorBgColour' is 'Set'
--
--     The cursor background color becomes the color you 'Set', while the cursor
--     foreground color doesn't change from the letter it is over.  For instance,
--     imagine there is a letter on the screen with a black background and a
--     green foreground.  If you bring the cursor overtop of it, the cursor
--     background will be the color you have 'Set', while the cursor foreground
--     will be green.
--
--     This is completely usable, but is slightly annoying if you place the cursor
--     over a letter with the same foreground color as the cursor's background
--     color, because the letter will not be readable. For instance, imagine you
--     have set your cursor background color to red, and somewhere on the screen
--     there is a letter with a black background and a red foreground. If you move
--     your cursor over the letter, the background of the cursor will be red (as
--     you have set), and the cursor foreground will be red (to match the original
--     foreground color of the letter). This will make it so you can't
--     actually read the letter, because the foreground and background are both
--     red.
--
-- * 'cursorFgColour' is 'Unset' and 'cursorBgColour' is 'Unset'
--
--     This combination makes the cursor inverse of whatever text it is over.
--     If your cursor is over red text with a black background, the cursor
--     background will be red and the cursor foreground will be black.
--
--     This is the default.
--
-- 'cursorFgColour' is not supported in @vte-2.91@ versions older than 0.44.
-- (This is somewhat confusing. Note that @vte-2.91@ is the name of the system
-- library, and @0.44@ is its version number.)
--
-- See 'defaultColourConfig' for the defaults for 'ColourConfig' used in Termonad.
data ColourConfig c = ColourConfig
  { cursorFgColour :: !(Option c)
    -- ^ Foreground color of the cursor.  This is the color of the text that
    -- the cursor is over.  This is not supported on older versions of VTE.
  , cursorBgColour :: !(Option c)
    -- ^ Background color of the cursor.  This is the color of the cursor
    -- itself.
  , foregroundColour :: !(Option c)
    -- ^ Color of the default foreground text in the terminal.
  , backgroundColour :: !(Option c)
    -- ^ Background color for the terminal
  , palette :: !(Palette c)
    -- ^ Color palette for the terminal.  See 'Palette'.
  } deriving (Eq, Show, Functor)

-- | Default setting for a 'ColourConfig'.  The cursor colors, font foreground
-- color, background color, and color palette are all left at the defaults set
-- by VTE.
--
-- >>> defaultColourConfig
-- ColourConfig {cursorFgColour = Unset, cursorBgColour = Unset, foregroundColour = Unset, backgroundColour = Unset, palette = NoPalette}
defaultColourConfig :: ColourConfig (AlphaColour Double)
defaultColourConfig = ColourConfig
  { cursorFgColour = Unset
  , cursorBgColour = Unset
  , foregroundColour = Unset
  , backgroundColour = Unset
  , palette = NoPalette
  }

$(makeLensesFor
    [ ("cursorFgColour", "lensCursorFgColour")
    , ("cursorBgColour", "lensCursorBgColour")
    , ("foregroundColour", "lensForegroundColour")
    , ("backgroundColour", "lensBackgroundColour")
    , ("palette", "lensPalette")
    ]
    ''ColourConfig
 )

------------------------------
-- ConfigExtension Instance --
------------------------------

-- | Extension that allows setting colors for terminals in Termonad.
data ColourExtension = ColourExtension
  { colourExtConf :: MVar (ColourConfig (AlphaColour Double))
    -- ^ 'MVar' holding the current 'ColourConfig'.  This could potentially be
    -- passed to other extensions or user code.  This would allow changing the
    -- colors for new terminals in realtime.
  , colourExtCreateTermHook :: TMState -> Terminal -> IO ()
    -- ^ The 'createTermHook' used by the 'ColourExtension'.  This sets the
    -- colors for a new terminal based on the 'ColourConfig' in 'colourExtConf'.
  }

-- | The default 'createTermHook' for 'colourExtCreateTermHook'.  Set the colors
-- for a terminal based on the given 'ColourConfig'.
colourHook :: MVar (ColourConfig (AlphaColour Double)) -> TMState -> Terminal -> IO ()
colourHook mvarColourConf _ vteTerm = do
  colourConf <- readMVar mvarColourConf
  let paletteColourList = paletteToList $ palette colourConf
  rgbaPaletteColourList <- traverse colourToRgba paletteColourList
  terminalSetColors vteTerm Nothing Nothing (Just rgbaPaletteColourList)
  whenSet (backgroundColour colourConf) $
    terminalSetColorBackground vteTerm <=< colourToRgba
  whenSet (foregroundColour colourConf) $
    terminalSetColorForeground vteTerm <=< colourToRgba
  whenSet (cursorBgColour colourConf) $
    terminalSetColorCursor vteTerm . Just <=< colourToRgba
#ifdef VTE_VERSION_GEQ_0_44
  whenSet (cursorFgColour colourConf) $
    terminalSetColorCursorForeground vteTerm . Just <=< colourToRgba
#endif

colourToRgba :: AlphaColour Double -> IO RGBA
colourToRgba colour = do
  let RGB red green blue = toSRGB $ pureColour colour
      alpha = alphaChannel colour
  rgba <- newZeroRGBA
  setRGBARed rgba red
  setRGBAGreen rgba green
  setRGBABlue rgba blue
  setRGBAAlpha rgba alpha
  pure rgba

-- | Create a 'ColourExtension' based on a given 'ColourConfig'.
--
-- Most users will want to use this.
createColourExtension :: ColourConfig (AlphaColour Double) -> IO ColourExtension
createColourExtension conf = do
  mvarConf <- newMVar conf
  pure $
    ColourExtension
      { colourExtConf = mvarConf
      , colourExtCreateTermHook = colourHook mvarConf
      }

-- | Create a 'ColourExtension' based on 'defaultColourConfig'.
--
-- Note that this is not needed if you just want to use the default colors for
-- Termonad.  However, if you want to pass around the 'MVar' 'ColourConfig' for
-- extensions to use, then you may need this function.
createDefColourExtension :: IO ColourExtension
createDefColourExtension = createColourExtension defaultColourConfig

-- | Add a given 'ColourConfig' to a 'TMConfig'.  This adds 'colourHook' to the
-- 'createTermHook' in 'TMConfig'.
addColourConfig :: TMConfig -> ColourConfig (AlphaColour Double) -> IO TMConfig
addColourConfig tmConf colConf = do
  ColourExtension _ newHook <- createColourExtension colConf
  let newTMConf = tmConf & lensHooks . lensCreateTermHook %~ addColourHook newHook
  pure newTMConf

-- | This is similar to 'addColourConfig', but can be used on a
-- 'ColourExtension' created with 'createColourExtension'.
addColourExtension :: TMConfig -> ColourExtension -> TMConfig
addColourExtension tmConf (ColourExtension _ newHook) =
  tmConf & lensHooks . lensCreateTermHook %~ addColourHook newHook

-- | This function shows how to combine 'createTermHook's.
--
-- This first runs the old hook, followed by the new hook.
--
-- This is used internally by 'addColourConfig' and 'addColourExtension'.
addColourHook
  :: (TMState -> Terminal -> IO ()) -- ^ New hook
  -> (TMState -> Terminal -> IO ()) -- ^ Old hook
  -> TMState
  -> Terminal
  -> IO ()
addColourHook newHook oldHook tmState term = do
  oldHook tmState term
  newHook tmState term
