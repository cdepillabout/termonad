{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Termonad.Config where

import Termonad.Prelude hiding ((\\), index)

import Control.Lens (makeLensesFor, makePrisms)
import Data.Colour (Colour, affineCombo)
import Data.Colour.Names (black)
import Data.Colour.SRGB (sRGB24)
import Data.Type.Combinator (I(..), Uncur3(..))
import Data.Type.Vector
  (VecT(..), Vec, M(..), vgen_, mgen_, onTail, tail', index , Matrix, onMatrix)
import Data.Type.Product (Prod(..))
import Data.Type.Fin (Fin(..), fin)
import Data.Type.Fin.Indexed (IFin(..))
import Data.Type.Nat (Nat(..))
import Type.Class.Witness ((\\))
import Type.Class.Known (Known(..))
import Type.Family.Nat (N(..), N3, N6, N8, type (+))
import Type.Family.List (Fsts3, Thds3)

import qualified Data.Foldable
import GI.Vte (CursorBlinkMode(CursorBlinkModeOn))

----------------------
-- Orphan Instances --
----------------------

-- These should eventually be provided by type-combinators.

deriving instance Functor  (Matrix ns) => Functor  (M ns)
deriving instance Foldable (Matrix ns) => Foldable (M ns)

instance Applicative (Matrix ns) => Applicative (M ns) where
  pure = M . pure
  M f <*> M a = M $ f <*> a

instance Monad (Matrix ns) => Monad (M ns) where
  M ma >>= f = M (ma >>= getMatrix . f)

instance Known (IFin ('S n)) 'Z where
  known = IFZ

instance Known (IFin n) m => Known (IFin ('S n)) ('S m) where
  type KnownC (IFin ('S n)) ('S m) = Known (IFin n) m
  known = IFS known

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

data Palette c
  = NoPalette
  | BasicPalette !(Vec N8 c)
  | ExtendedPalette !(Vec N8 c) !(Vec N8 c)
  | ColourCubePalette !(Vec N8 c) !(Vec N8 c) !(M [N6, N6, N6] c)
  | FullPalette !(Vec N8 c) !(Vec N8 c) !(M [N6, N6, N6] c) !(Vec N24 c)
  deriving (Eq, Show, Functor, Foldable)

type N24 = N8 + N8 + N8

pattern EmptyV :: VecT 'Z f c
pattern EmptyV = ØV

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

--------------------------
-- Misc VecT Operations --
--------------------------

-- These are waiting to be upstreamed at
-- https://github.com/kylcarte/type-combinators/pull/11.

onHead :: (f a -> f a) -> VecT ('S n) f a -> VecT ('S n) f a
onHead f (a :* as) = f a :* as

take' :: IFin ('S n) m -> VecT n f a -> VecT m f a
take' = \case
  IFS n -> onTail (take' n) \\ n
  IFZ   -> const EmptyV

drop' :: Nat m -> VecT (m + n) f a -> VecT n f a
drop' = \case
  S_ n -> drop' n . tail'
  Z_   -> id

asM :: (M ms a -> M ns a) -> (Matrix ms a -> Matrix ns a)
asM f = getMatrix . f . M

mIndex :: Prod Fin ns -> M ns a -> a
mIndex = \case
  i :< is -> mIndex is . onMatrix (index i)
  Ø       -> getI . getMatrix

deIndex :: IFin n m -> Fin n
deIndex = \case
  IFS n -> FS (deIndex n)
  IFZ   -> FZ

vUpdateAt :: Fin n -> (f a -> f a) -> VecT n f a -> VecT n f a
vUpdateAt = \case
  FS m -> onTail . vUpdateAt m
  FZ   -> onHead

vSetAt :: Fin n -> f a -> VecT n f a -> VecT n f a
vSetAt n = vUpdateAt n . const

vSetAt' :: Fin n -> a -> Vec n a -> Vec n a
vSetAt' n = vSetAt n . I

mUpdateAt :: Prod Fin ns -> (a -> a) -> M ns a -> M ns a
mUpdateAt = \case
  n :< ns -> onMatrix . vUpdateAt n . asM . mUpdateAt ns
  Ø       -> (<$>)

mSetAt :: Prod Fin ns -> a -> M ns a -> M ns a
mSetAt ns = mUpdateAt ns . const

data Range n l m = Range (IFin ('S n) l) (IFin ('S n) (l + m))
  deriving (Show, Eq)

instance (Known (IFin ('S n)) l, Known (IFin ('S n)) (l + m))
  => Known (Range n l) m where
  type KnownC (Range n l) m
    = (Known (IFin ('S n)) l, Known (IFin ('S n)) (l + m))
  known = Range known known

updateRange :: Range n l m -> (Fin m -> f a -> f a) -> VecT n f a -> VecT n f a
updateRange = \case
  Range  IFZ     IFZ    -> \_ -> id
  Range (IFS l) (IFS m) -> \f -> onTail (updateRange (Range l m) f) \\ m
  Range  IFZ    (IFS m) -> \f -> onTail (updateRange (Range IFZ m) $ f . FS)
                               . onHead (f FZ) \\ m

setRange :: Range n l m -> VecT m f a -> VecT n f a -> VecT n f a
setRange r nv = updateRange r (\i _ -> index i nv)

updateSubmatrix
  :: (ns ~ Fsts3 nlms, ms ~ Thds3 nlms)
  => Prod (Uncur3 Range) nlms -> (Prod Fin ms -> a -> a) -> M ns a -> M ns a
updateSubmatrix = \case
  Ø              -> \f -> (f Ø <$>)
  Uncur3 r :< rs -> \f -> onMatrix . updateRange r $ \i ->
    asM . updateSubmatrix rs $ f . (i :<)

setSubmatrix
  :: (ns ~ Fsts3 nlms, ms ~ Thds3 nlms)
  => Prod (Uncur3 Range) nlms -> M ms a -> M ns a -> M ns a
setSubmatrix rs sm = updateSubmatrix rs $ \is _ -> mIndex is sm

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
