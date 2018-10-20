{-# LANGUAGE TemplateHaskell #-}

-- | Module    : Termonad.Config.Colour
-- Description : Termonad Configuration Colour Options
-- Copyright   : (c) Dennis Gosnell, 2018
-- License     : BSD3
-- Stability   : experimental
-- Portability : POSIX
--
-- To use this config extension in your @~/.config/termonad/termonad.hs@, first
-- import this module. Then you can simply add the new configuration options to
-- an existing 'TMConfig' with the '<+>' operator, though note that the
-- 'TMConfig' must come first (any other way is a type error).
--
-- E.g.
--
-- > import "Termonad
-- > import "Termonad.Config.Colour"
-- > import "Termonad.Config.Vec" ('Vec', 'VecT'((':+'), 'N8', 'EmptyV'))
-- > import "Data.Colour.SRGB" ('Colour', 'sRGB24')
-- >
-- > myBasicConfig :: 'TMConfig'
-- > myBasicConfig = 'defaultTMConfig'
-- >   { 'showScrollbar' = 'ShowScrollbarNever'
-- >   , 'confirmExit' = False
-- >   , 'showMenu' = False
-- >   , 'cursorBlinkMode' = 'CursorBlinkModeOff'
-- >   }
-- >
-- > myColourConfig :: 'ColourConfig' ('Colour' Double)
-- > myColourConfig = 'defaultColourConfig'
-- >   { 'cursorBgColour' = 'Set' ('sRGB24' 120 80 110)
-- >   , 'foregroundColour' = 'sRGB24' 220 180 210
-- >   , 'palette' = 'BasicPalette' myStandardColours
-- >   } where
-- >       myStandardColours :: 'Vec' 'N8' ('Colour' Double)
-- >       myStandardColours
-- >         =  'sRGB24'  40  30  20
-- >         ':+' 'sRGB24' 180  30  20
-- >         ':+' 'sRGB24'  40 160  20
-- >         ':+' 'sRGB24' 180 160  20
-- >         ':+' 'sRGB24'  40  30 120
-- >         ':+' 'sRGB24' 180  30 120
-- >         ':+' 'sRGB24'  40 160 120
-- >         ':+' 'sRGB24' 180 160 120
-- >         ':+' 'EmptyV'
-- >
-- > main :: IO ()
-- > main = start (myBasicConfig <+> myColourConfig)

module Termonad.Config.Colour where

import Termonad.Prelude hiding ((\\), index)

import Control.Lens (makeLensesFor)
import Data.Colour (Colour, black, affineCombo)
import Data.Colour.SRGB (RGB(RGB), toSRGB, sRGB24, sRGB24show)
import qualified Data.Foldable
import Data.Type.Combinator (I(..))
import Data.Type.Fin (Fin(..), fin)
import Data.Type.Product (Prod(..))
import Data.Type.Vector
  (VecT(..), Vec, M(..), vgen_, mgen_)
import GI.Gdk (RGBA, newZeroRGBA, setRGBABlue, setRGBAGreen, setRGBARed)
import GI.Vte
  ( Terminal
  , terminalSetColors
  , terminalSetColorCursor
  , terminalSetColorCursorForeground
--, terminalSetColorBackground
  , terminalSetColorForeground
  )
import Text.Show (showString)
import Type.Family.Nat (N3, N6, N8)

import Termonad.App (getFocusedTermFromState)
import Termonad.Config.Vec
import Termonad.Config.Extension (fromMessage)
import Termonad.Types
  ( ConfigExtension(..)
  , ConfigHooks(createTermHook)
  , Message
  , Option(Unset)
  , TMState
  , defaultConfigHooks
  , whenSet
  )

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
coloursFromBits :: forall b. (Ord b, Floating b) => Word8 -> Word8 -> Vec N8 (Colour b)
coloursFromBits scale offset = vgen_ createElem
  where
    createElem :: Fin N8 -> I (Colour b)
    createElem finN =
      let red = cmp 0 finN
          green = cmp 1 finN
          blue = cmp 2 finN
          color = sRGB24 red green blue
      in I color

    cmp :: Int -> Fin N8 -> Word8
    cmp i = (offset +) . (scale *) . fromIntegral . bit i . fin

    bit :: Int -> Int -> Int
    bit m i = i `div` (2 ^ m) `mod` 2

-- | A 'Vec' of standard colors.  Default value for 'BasicPalette'.
--
-- >>> showColourVec defaultStandardColours
-- ["#000000","#c00000","#00c000","#c0c000","#0000c0","#c000c0","#00c0c0","#c0c0c0"]
defaultStandardColours :: (Ord b, Floating b) => Vec N8 (Colour b)
defaultStandardColours = coloursFromBits 192 0

-- | A 'Vec' of extended (light) colors.  Default value for 'ExtendedPalette'.
--
-- >>> showColourVec defaultLightColours
-- ["#3f3f3f","#ff3f3f","#3fff3f","#ffff3f","#3f3fff","#ff3fff","#3fffff","#ffffff"]
defaultLightColours :: (Ord b, Floating b) => Vec N8 (Colour b)
defaultLightColours = coloursFromBits 192 63

-- | A helper function for showing all the colors in 'Vec' of colors.
showColourVec :: forall n. Vec n (Colour Double) -> [String]
showColourVec = fmap sRGB24show . Data.Foldable.toList

-- | Specify a colour cube with one colour vector for its displacement and three
-- colour vectors for its edges. Produces a uniform 6x6x6 grid bounded by
-- and orthognal to the faces.
cube
  :: Fractional b => Colour b -> Vec N3 (Colour b) -> M '[N6, N6, N6] (Colour b)
cube d (I i :* I j :* I k :* ØV) = mgen_ $ \(x :< y :< z :< Ø) ->
  affineCombo [(1, d), (coef x, i), (coef y, j), (coef z, k)] black
  where coef n = fromIntegral (fin n) / 5

-- | A matrix of a 6 x 6 x 6 color cube. Default value for 'ColourCubePalette'.
--
-- >>> putStrLn $ pack $ showColourCube defaultColourCube
-- [ [ #000000, #00005f, #000087, #0000af, #0000d7, #0000ff
--   , #005f00, #005f5f, #005f87, #005faf, #005fd7, #005fff
--   , #008700, #00875f, #008787, #0087af, #0087d7, #0087ff
--   , #00af00, #00af5f, #00af87, #00afaf, #00afd7, #00afff
--   , #00d700, #00d75f, #00d787, #00d7af, #00d7d7, #00d7ff
--   , #00ff00, #00ff5f, #00ff87, #00ffaf, #00ffd7, #00ffff
--   ]
-- , [ #5f0000, #5f005f, #5f0087, #5f00af, #5f00d7, #5f00ff
--   , #5f5f00, #5f5f5f, #5f5f87, #5f5faf, #5f5fd7, #5f5fff
--   , #5f8700, #5f875f, #5f8787, #5f87af, #5f87d7, #5f87ff
--   , #5faf00, #5faf5f, #5faf87, #5fafaf, #5fafd7, #5fafff
--   , #5fd700, #5fd75f, #5fd787, #5fd7af, #5fd7d7, #5fd7ff
--   , #5fff00, #5fff5f, #5fff87, #5fffaf, #5fffd7, #5fffff
--   ]
-- , [ #870000, #87005f, #870087, #8700af, #8700d7, #8700ff
--   , #875f00, #875f5f, #875f87, #875faf, #875fd7, #875fff
--   , #878700, #87875f, #878787, #8787af, #8787d7, #8787ff
--   , #87af00, #87af5f, #87af87, #87afaf, #87afd7, #87afff
--   , #87d700, #87d75f, #87d787, #87d7af, #87d7d7, #87d7ff
--   , #87ff00, #87ff5f, #87ff87, #87ffaf, #87ffd7, #87ffff
--   ]
-- , [ #af0000, #af005f, #af0087, #af00af, #af00d7, #af00ff
--   , #af5f00, #af5f5f, #af5f87, #af5faf, #af5fd7, #af5fff
--   , #af8700, #af875f, #af8787, #af87af, #af87d7, #af87ff
--   , #afaf00, #afaf5f, #afaf87, #afafaf, #afafd7, #afafff
--   , #afd700, #afd75f, #afd787, #afd7af, #afd7d7, #afd7ff
--   , #afff00, #afff5f, #afff87, #afffaf, #afffd7, #afffff
--   ]
-- , [ #d70000, #d7005f, #d70087, #d700af, #d700d7, #d700ff
--   , #d75f00, #d75f5f, #d75f87, #d75faf, #d75fd7, #d75fff
--   , #d78700, #d7875f, #d78787, #d787af, #d787d7, #d787ff
--   , #d7af00, #d7af5f, #d7af87, #d7afaf, #d7afd7, #d7afff
--   , #d7d700, #d7d75f, #d7d787, #d7d7af, #d7d7d7, #d7d7ff
--   , #d7ff00, #d7ff5f, #d7ff87, #d7ffaf, #d7ffd7, #d7ffff
--   ]
-- , [ #ff0000, #ff005f, #ff0087, #ff00af, #ff00d7, #ff00ff
--   , #ff5f00, #ff5f5f, #ff5f87, #ff5faf, #ff5fd7, #ff5fff
--   , #ff8700, #ff875f, #ff8787, #ff87af, #ff87d7, #ff87ff
--   , #ffaf00, #ffaf5f, #ffaf87, #ffafaf, #ffafd7, #ffafff
--   , #ffd700, #ffd75f, #ffd787, #ffd7af, #ffd7d7, #ffd7ff
--   , #ffff00, #ffff5f, #ffff87, #ffffaf, #ffffd7, #ffffff
--   ]
-- ]
defaultColourCube :: (Ord b, Floating b) => M '[N6, N6, N6] (Colour b)
defaultColourCube = mgen_ $ \(x :< y :< z :< Ø) -> sRGB24 (cmp x) (cmp y) (cmp z)
  where
    cmp :: Fin N6 -> Word8
    cmp i =
      let i' = fromIntegral (fin i)
      in signum i' * 55 + 40 * i'

-- | Helper function for showing all the colors in a color cube. This is used
-- for debugging.
showColourCube :: M '[N6, N6, N6] (Colour Double) -> String
showColourCube (M matrix) =
  -- TODO: This function will only work with a 6x6x6 matrix, but it could be
  -- generalized to work with any Rank-3 matrix.
  let itemList = Data.Foldable.toList matrix
  in showSColourCube itemList ""
  where
    showSColourCube :: [Colour Double] -> String -> String
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

    showSquare :: Int -> [Colour Double] -> String -> String
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

    showRow :: Int -> Int -> [Colour Double] -> String -> String
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

    showCol :: Colour Double -> String -> String
    showCol col str = sRGB24show col <> str

-- | A 'Vec' of a grey scale.  Default value for 'FullPalette'.
--
-- >>> showColourVec defaultGreyscale
-- ["#080808","#121212","#1c1c1c","#262626","#303030","#3a3a3a","#444444","#4e4e4e","#585858","#626262","#6c6c6c","#767676","#808080","#8a8a8a","#949494","#9e9e9e","#a8a8a8","#b2b2b2","#bcbcbc","#c6c6c6","#d0d0d0","#dadada","#e4e4e4","#eeeeee"]
defaultGreyscale :: (Ord b, Floating b) => Vec N24 (Colour b)
defaultGreyscale = vgen_ $ \n -> I $
  let l = 8 + 10 * fromIntegral (fin n)
  in sRGB24 l l l

-- | The configuration for the colors used by Termonad.
--
-- 'foregroundColour' and 'backgroundColour' allow you to set the color of the
-- foreground text and background of the terminal (although see the __WARNING__
-- below).  Most people use a black background and a light foreground for their
-- terminal, so this is the default.
--
-- 'palette' allows you to set the full color palette used by the terminal.
-- See 'Palette' for more information.
--
-- If you want to use a terminal with a white (or light) background and a black
-- foreground, it may be a good idea to change some of the colors in the
-- 'Palette' as well.
--
-- (__WARNING__: Currently due to issues either with VTE or the bindings generated for
-- Haskell, background colour cannot be set independently of the palette.
-- The @backgroundColour@ field will be ignored and the 0th colour in the
-- palette (by default black) will be used as the background colour. See
-- <https://github.com/cdepillabout/termonad/issues/29 this issue>.
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
--   This is completely usable, but is slightly annoying if you place the cursor
--   over a letter with the same foreground color as the cursor's background
--   color, because the letter will not be readable. For instance, imagine you
--   have set your cursor background color to red, and somewhere on the screen
--   there is a letter with a black background and a red foreground. If you move
--   your cursor over the letter, the background of the cursor will be red (as
--   you have set), and the cursor foreground will be red (to match the original
--   foreground color of the letter). This will make it so you can't
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
--
-- See 'defaultColourConfig' for the defaults for 'ColourConfig' used in Termonad.
data ColourConfig c = ColourConfig
  { cursorFgColour :: !(Option c) -- ^ Foreground color of the cursor.  This is
                                  -- the color of the text that the cursor is
                                  -- over.
  , cursorBgColour :: !(Option c) -- ^ Background color of the cursor.  This is
                                  -- the color of the cursor itself.
  , foregroundColour :: !c -- ^ Color of the default default foreground text in
                           -- the terminal.
  , backgroundColour :: !c -- ^ Background color for the terminal, however, See
                           -- the __WARNING__ above.
  , palette :: !(Palette c) -- ^ Color palette for the terminal.  See 'Palette'.
  } deriving (Eq, Show, Functor)

-- | Default setting for a 'ColourConfig'.  The cursor colors are left at their
-- default for VTE.  The foreground text for the terminal is grey and the
-- background of the terminal is black.  The palette is left as the default for
-- VTE.
--
-- >>> let fgGrey = sRGB24 192 192 192
-- >>> let bgBlack = sRGB24 0 0 0
-- >>> let defCC = ColourConfig { cursorFgColour = Unset, cursorBgColour = Unset, foregroundColour = fgGrey, backgroundColour = bgBlack, palette = NoPalette }
-- >>> defaultColourConfig == defCC
-- True
defaultColourConfig :: ColourConfig (Colour Double)
defaultColourConfig = ColourConfig
  { cursorFgColour = Unset
  , cursorBgColour = Unset
  , foregroundColour = sRGB24 192 192 192
  , backgroundColour = black
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

-- | Messages for runtime interaction with the colour configuration.
data ColourMessage
  -- | This message can be sent to update the colours in use on the current term,
  --   and the default colours for any new terms.
  = UpdateColours (ColourConfig (Colour Double) -> ColourConfig (Colour Double))

instance Message ColourMessage

instance ConfigExtension (ColourConfig (Colour Double)) where
  hooks :: ColourConfig (Colour Double) -> ConfigHooks
  hooks colourConf = defaultConfigHooks
    { createTermHook = \_ vteTerm -> implementColourConfig colourConf vteTerm }

  message :: Message m => TMState -> m -> ColourConfig (Colour Double) -> IO (ColourConfig (Colour Double))
  message mvarTMState m colourConf = case fromMessage m of
    Nothing -> do
      putStrLn "ColourExtension: Failed to catch message."
      pure colourConf
    Just (UpdateColours f) -> do
      putStrLn "ColourExtension: Caught UpdateColours message."
      let colourConf' = f colourConf
      mTerm <- getFocusedTermFromState mvarTMState
      whenJust mTerm (implementColourConfig colourConf')
      pure colourConf'

implementColourConfig :: ColourConfig (Colour Double) -> Terminal -> IO ()
implementColourConfig colourConf vteTerm = do
  terminalSetColors vteTerm Nothing Nothing . Just
    =<< traverse toRGBA (paletteToList . palette $ colourConf)
  -- PR#28/IS#29: Setting the background colour is broken in gi-vte or VTE.  If
  -- this next line is called, then you are no longer able to set the
  -- background color using the palette.
  -- terminalSetColorBackground vteTerm =<< toRGBA (backgroundColour colourConf)
  terminalSetColorForeground vteTerm =<< toRGBA (foregroundColour colourConf)
  let optPerform setC cField = whenSet (cField colourConf) $ \c ->
        setC vteTerm . Just =<< toRGBA c
  optPerform terminalSetColorCursor cursorBgColour
  optPerform terminalSetColorCursorForeground cursorFgColour
  where
    toRGBA :: Colour Double -> IO RGBA
    toRGBA colour = do
      let RGB red green blue = toSRGB colour
      rgba <- newZeroRGBA
      setRGBARed rgba red
      setRGBAGreen rgba green
      setRGBABlue rgba blue
      pure rgba
