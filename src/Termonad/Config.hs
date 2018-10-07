{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Termonad.Config where

-- --< Imports >-- {{{

import Termonad.Prelude hiding ((\\), index)

import Control.Lens (makeLensesFor, makePrisms)
import Data.Colour (Colour, affineCombo, blend)
import Data.Colour.Names (black, white)
import Data.Colour.SRGB (sRGB24, sRGB)
import Data.Type.Combinator (I(..), Uncur3(..))
import Data.Type.Vector
  ( VecT(..), Vec, pattern (:+), M(..), vgen_, mgen_, onTail, tail', index
  , Matrix, onMatrix )
import Data.Type.Product (Prod(..))
import Data.Type.Fin (Fin(..), fin)
import Data.Type.Fin.Indexed (IFin(..))
import Data.Type.Nat (Nat(..))
import Type.Class.Witness ((\\))
import Type.Class.Known (Known(..))
import Type.Family.Nat (N(..), N3, N6, N8, type (+))
import Type.Family.List (Fsts3, Thds3)

import qualified Data.Foldable

-- }}}

-- --< Orphan instances >-- {{{
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

-- }}}

-- --< FontConfig >-- {{{

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

-- }}}

-- --< ColourConfig >-- {{{

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

defaultStandardColours :: (Ord b, Floating b) => Vec N8 (Colour b)
defaultStandardColours
   = sRGB24   0   0   0 -- 00: black
  :+ sRGB24 192   0   0 -- 01: red
  :+ sRGB24   0 192   0 -- 02: green
  :+ sRGB24 192 192   0 -- 03: yellow
  :+ sRGB24   0   0 192 -- 04: blue
  :+ sRGB24 192   0 192 -- 05: purple
  :+ sRGB24   0 192 192 -- 06: cyan
  :+ sRGB24 192 192 192 -- 07: lightgrey
  :+ EmptyV

defaultLightColours :: (Ord b, Floating b) => Vec N8 (Colour b)
defaultLightColours = (<> sRGB24 63 63 63) <$> defaultStandardColours

-- | Specify a colour cube with one colour vector for its displacement and three
--   colour vectors for its edges. Produces a uniform 6x6x6 grid bounded by
--   and orthognal to the faces.
cube
  :: Fractional b => Colour b -> Vec N3 (Colour b) -> M [N6, N6, N6] (Colour b)
cube d (I i :* I j :* I k :* ØV) = mgen_ $ \(x :< y :< z :< Ø) ->
  affineCombo [(1, d), (coef x, i), (coef y, j), (coef z, k)] black
  where coef n = fromIntegral (fin n) / 5

defaultColourCube :: (Ord b, Floating b) => M [N6, N6, N6] (Colour b)
defaultColourCube
  = cube black (sRGB 1 0 0 :+ sRGB 0 1 0 :+ sRGB 0 0 1 :+ EmptyV)

defaultGreyscale :: (Ord b, Floating b) => Vec N24 (Colour b)
defaultGreyscale = vgen_ $ \n -> I $ blend (beta n) white black
  where beta n = (fromIntegral (fin n) / 23) ** 2

-- | NB: Currently due to issues either with VTE or the bindings generated for
--   Haskell, background colour cannot be set independently of the palette.
--   The @backgroundColour@ field will be ignored and the 0th colour in the
--   palette (usually black) will be used as the background colour.
data ColourConfig c = ColourConfig
  { cursorColour :: !c
  , foregroundColour :: !c
  , backgroundColour :: !c
  , palette :: !(Palette c)
  } deriving (Eq, Show, Functor)

$(makeLensesFor
    [ ("cursorColour", "lensCursorColour")
    , ("foregroundColour", "lensForegroundColour")
    , ("backgroundColour", "lensBackgroundColour")
    , ("palette", "lensPalette")
    ]
    ''ColourConfig
 )

defaultColourConfig :: (Ord b, Floating b) => ColourConfig (Colour b)
defaultColourConfig = ColourConfig
  { cursorColour = sRGB24 192 192 192
  , foregroundColour = sRGB24 192 192 192
  , backgroundColour = sRGB24 0 0 0
  , palette = NoPalette
  }

-- }}}

-- --< VecT operations >-- {{{
-- These should be upstreamed.

-- --< Misc >-- {{{

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

-- }}}

-- --< Update/Set at index >-- {{{

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

-- }}}

-- --< Update/Set over range >-- {{{

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

-- }}}

-- }}}

-- --< Scrollbar >-- {{{

data ShowScrollbar
  = ShowScrollbarNever
  | ShowScrollbarAlways
  | ShowScrollbarIfNeeded
  deriving (Eq, Show)

-- }}}

-- --< TabBar >-- {{{

data ShowTabBar
  = ShowTabBarNever
  | ShowTabBarAlways
  | ShowTabBarIfNeeded
  deriving (Eq, Show)

-- }}}

-- --< TMConfig >-- {{{

data TMConfig = TMConfig
  { fontConfig :: !FontConfig
  , showScrollbar :: !ShowScrollbar
  , colourConfig :: !(ColourConfig (Colour Double))
  , scrollbackLen :: !Integer
  , confirmExit :: !Bool
  , wordCharExceptions :: !Text
  , showMenu :: !Bool
  , showTabBar :: !ShowTabBar
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
    }

-- }}}

