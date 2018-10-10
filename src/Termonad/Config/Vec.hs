{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Termonad.Config.Vec
  ( Fin
  , I(I)
  , Length
  , M(M)
  , Matrix
  , N3
  , N24
  , N6
  , N8
  , Prod((:<), Ø)
  , Vec
  , VecT((:*), ØV)
  , fin
  , known
  , mgen_
  , ppMatrix'
  , vgen_
  ) where

import Termonad.Prelude hiding ((\\), index)

import Data.Type.Combinator (I(..), Uncur3(..))
import Data.Type.Fin (Fin(..), fin)
import Data.Type.Fin.Indexed (IFin(..))
import Data.Type.Length (Length)
import Data.Type.Nat (Nat(..))
import Data.Type.Product (Prod(..))
import Data.Type.Vector
  ( M(M, getMatrix)
  , Matrix
  , Vec
  , VecT(..)
  , index
  , mgen_
  , onMatrix
  , onTail
  , ppMatrix'
  , tail'
  , vgen_
  )
import Type.Class.Known (Known(KnownC, known))
import Type.Class.Witness ((\\))
import Type.Family.List (Fsts3, Thds3)
import Type.Family.Nat (N(..), N3, N6, N8, type (+))

----------------------
-- Orphan Instances --
----------------------

-- These should eventually be provided by type-combinators.
-- See https://github.com/kylcarte/type-combinators/pull/11.

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

---------------------------------
-- Vector Helpers for Termonad --
---------------------------------

type N24 = N8 + N8 + N8

pattern EmptyV :: VecT 'Z f c
pattern EmptyV = ØV

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
