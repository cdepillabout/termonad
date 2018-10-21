{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Termonad.Config.Vec
  -- ( Fin
  -- , I(I)
  -- , M(M)
  -- , N3
  -- , N24
  -- , N6
  -- , N8
  -- , Prod((:<), Ø)
  -- , Range
  -- , Vec
  -- , VecT((:+), (:*), ØV, EmptyV)
  -- , fin
  -- , mgen_
  -- , setSubmatrix
  -- , vgen_
  -- , vSetAt'
  -- )
    where

import Termonad.Prelude hiding ((\\), index)

import qualified Data.Foldable as Data.Foldable
import Data.Kind (Type)
import Data.Singletons.Prelude
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Show
import Data.Singletons.TypeLits
import Data.Singletons.TH

-- import Data.Type.Combinator (I(..), Uncur3(..))
-- import Data.Type.Fin (Fin(..), fin)
-- import Data.Type.Fin.Indexed (IFin(..))
-- import Data.Type.Length (Length)
-- import Data.Type.Nat (Nat(..))
-- import Data.Type.Product (Prod(..))
-- import Data.Type.Vector
--   ( M(M, getMatrix)
--   , Matrix
--   , Vec
--   , VecT(..)
--   , pattern (:+)
--   , index
--   , mgen_
--   , onMatrix
--   , onTail
--   , ppMatrix'
--   , tail'
--   , vgen_
--   )
-- import Type.Class.Known (Known(KnownC, known))
-- import Type.Class.Witness ((\\))
-- import Type.Family.List (Fsts3, Thds3)
-- import Type.Family.Nat (N(..), N3, N6, N8, type (+))

----------------------
-- Orphan Instances --
----------------------

-- These should eventually be provided by type-combinators.
-- See https://github.com/kylcarte/type-combinators/pull/11.

-- deriving instance Functor  (Matrix ns) => Functor  (M ns)
-- deriving instance Foldable (Matrix ns) => Foldable (M ns)

-- instance Applicative (Matrix ns) => Applicative (M ns) where
--   pure = M . pure
--   M f <*> M a = M $ f <*> a

-- instance Monad (Matrix ns) => Monad (M ns) where
--   M ma >>= f = M (ma >>= getMatrix . f)

-- instance Known (IFin ('S n)) 'Z where
--   known = IFZ

-- instance Known (IFin n) m => Known (IFin ('S n)) ('S m) where
--   type KnownC (IFin ('S n)) ('S m) = Known (IFin n) m
--   known = IFS known

---------------------------------
-- Vector Helpers for Termonad --
---------------------------------

-- type N24 = N8 + N8 + N8

-- pattern EmptyV :: VecT 'Z f c
-- pattern EmptyV = ØV

--------------------------
-- Misc VecT Operations --
--------------------------

-- These are waiting to be upstreamed at
-- https://github.com/kylcarte/type-combinators/pull/11.

-- onHead :: (f a -> f a) -> VecT ('S n) f a -> VecT ('S n) f a
-- onHead f (a :* as) = f a :* as

-- take' :: IFin ('S n) m -> VecT n f a -> VecT m f a
-- take' = \case
--   IFS n -> onTail (take' n) \\ n
--   IFZ   -> const EmptyV

-- drop' :: Nat m -> VecT (m + n) f a -> VecT n f a
-- drop' = \case
--   S_ n -> drop' n . tail'
--   Z_   -> id

-- asM :: (M ms a -> M ns a) -> (Matrix ms a -> Matrix ns a)
-- asM f = getMatrix . f . M

-- mIndex :: Prod Fin ns -> M ns a -> a
-- mIndex = \case
--   i :< is -> mIndex is . onMatrix (index i)
--   Ø       -> getI . getMatrix

-- deIndex :: IFin n m -> Fin n
-- deIndex = \case
--   IFS n -> FS (deIndex n)
--   IFZ   -> FZ

-- vUpdateAt :: Fin n -> (f a -> f a) -> VecT n f a -> VecT n f a
-- vUpdateAt = \case
--   FS m -> onTail . vUpdateAt m
--   FZ   -> onHead

-- vSetAt :: Fin n -> f a -> VecT n f a -> VecT n f a
-- vSetAt n = vUpdateAt n . const

-- vSetAt' :: Fin n -> a -> Vec n a -> Vec n a
-- vSetAt' n = vSetAt n . I

-- mUpdateAt :: Prod Fin ns -> (a -> a) -> M ns a -> M ns a
-- mUpdateAt = \case
--   n :< ns -> onMatrix . vUpdateAt n . asM . mUpdateAt ns
--   Ø       -> (<$>)

-- mSetAt :: Prod Fin ns -> a -> M ns a -> M ns a
-- mSetAt ns = mUpdateAt ns . const

-- data Range n l m = Range (IFin ('S n) l) (IFin ('S n) (l + m))
--   deriving (Show, Eq)

-- instance (Known (IFin ('S n)) l, Known (IFin ('S n)) (l + m))
--   => Known (Range n l) m where
--   type KnownC (Range n l) m
--     = (Known (IFin ('S n)) l, Known (IFin ('S n)) (l + m))
--   known = Range known known

-- updateRange :: Range n l m -> (Fin m -> f a -> f a) -> VecT n f a -> VecT n f a
-- updateRange = \case
--   Range  IFZ     IFZ    -> \_ -> id
--   Range (IFS l) (IFS m) -> \f -> onTail (updateRange (Range l m) f) \\ m
--   Range  IFZ    (IFS m) -> \f -> onTail (updateRange (Range IFZ m) $ f . FS)
--                                . onHead (f FZ) \\ m

-- setRange :: Range n l m -> VecT m f a -> VecT n f a -> VecT n f a
-- setRange r nv = updateRange r (\i _ -> index i nv)

-- updateSubmatrix
--   :: (ns ~ Fsts3 nlms, ms ~ Thds3 nlms)
--   => Prod (Uncur3 Range) nlms -> (Prod Fin ms -> a -> a) -> M ns a -> M ns a
-- updateSubmatrix = \case
--   Ø              -> \f -> (f Ø <$>)
--   Uncur3 r :< rs -> \f -> onMatrix . updateRange r $ \i ->
--     asM . updateSubmatrix rs $ f . (i :<)

-- setSubmatrix
--   :: (ns ~ Fsts3 nlms, ms ~ Thds3 nlms)
--   => Prod (Uncur3 Range) nlms -> M ms a -> M ns a -> M ns a
-- setSubmatrix rs sm = updateSubmatrix rs $ \is _ -> mIndex is sm


-----------
-- Peano --
-----------

$(singletons [d|

  data Peano = Z | S Peano deriving (Eq, Ord, Show)

  addPeano :: Peano -> Peano -> Peano
  addPeano Z a = a
  addPeano (S a) b = S (addPeano a b)

  subtractPeano :: Peano -> Peano -> Peano
  subtractPeano Z _ = Z
  subtractPeano a Z = a
  subtractPeano (S a) (S b) = subtractPeano a b

  multPeano :: Peano -> Peano -> Peano
  multPeano Z _ = Z
  multPeano (S a) b = addPeano (multPeano a b) b

  n0 :: Peano
  n0 = Z

  n1 :: Peano
  n1 = S n0

  n2 :: Peano
  n2 = S n1

  n3 :: Peano
  n3 = S n2

  n4 :: Peano
  n4 = S n3

  n5 :: Peano
  n5 = S n4

  n6 :: Peano
  n6 = S n5

  n7 :: Peano
  n7 = S n6

  n8 :: Peano
  n8 = S n7

  n9 :: Peano
  n9 = S n8

  n10 :: Peano
  n10 = S n9

  n24 :: Peano
  n24 = multPeano n4 n6

  -- fromIntegerPeano :: Integer -> Peano
  -- fromIntegerPeano n = if n <= 0 then Z else S (fromIntegerPeano (n - 1))

  -- fromNatPeano :: forall (n :: Nat). Sing n -> Peano
  -- fromNatPeano n = Z

  instance Num Peano where
    (+) = addPeano

    (-) = subtractPeano

    (*) = multPeano

    abs = id

    signum Z = Z
    signum (S _) = S Z

    fromInteger _ = error "fromInteger for Peano not supported"


  |])

---------
-- Fin --
---------

data Fin :: Peano -> Type where
  FZ :: forall (n :: Peano). Fin ('S n)
  FS :: forall (n :: Peano). !(Fin n) -> Fin ('S n)

deriving instance Eq (Fin n)
deriving instance Ord (Fin n)
deriving instance Show (Fin n)

----------
-- Prod --
----------

data Prod :: [Type] -> Type where
  EmptyProd :: Prod '[]
  ProdCons :: forall (a :: Type) (as :: [Type]). !a -> !(Prod as) -> Prod (a ': as)

-- | Infix operator for 'ProdCons.
pattern (:<) :: (a :: Type) -> Prod as -> Prod (a ': as)
pattern a :< prod = ProdCons a prod
infixr 6 :<

---------
-- Vec --
---------

data Vec (n :: Peano) :: Type -> Type where
  EmptyVec :: Vec 'Z a
  VecCons :: !a -> !(Vec n a) -> Vec ('S n) a
  deriving anyclass MonoFoldable

deriving instance Eq a => Eq (Vec n a)
deriving instance Ord a => Ord (Vec n a)
deriving instance Show a => Show (Vec n a)

deriving instance Functor (Vec n)
deriving instance Foldable (Vec n)

type instance Element (Vec n a) = a

-- | Infix operator for 'VecCons'.
pattern (:*) :: (a :: Type) -> Vec n a -> Vec ('S n) a
pattern a :* vec = VecCons a vec
infixr 6 :*

------------
-- Matrix --
------------

type family MatrixTF (ns :: [Peano]) (a :: Type) :: Type where
  MatrixTF '[] a = a
  MatrixTF (n ': ns) a = Vec n (Matrix ns a)

-- data MatrixTFSym1 (l :: TyFun
--                       [a6989586621679682896] a6989586621679682896)
--   = forall a1 (arg :: [a1]).
--     SameKind (Apply HeadSym0 arg) (HeadSym1 arg) =>
--     Data.Singletons.Prelude.List.HeadSym0KindInference
--   	-- Defined in ‘Data.Singletons.Prelude.List’
-- type instance Apply HeadSym0 l = Head l
--   	-- Defined in ‘Data.Singletons.Prelude.List’
    --
    --

-- type role MapSym0 phantom

-- data MapSym0 (l :: TyFun
--                      (TyFun a6989586621679662939 b6989586621679662940 -> *)
--                      (TyFun [a6989586621679662939] [b6989586621679662940] -> *))
--   = forall a1 b1 (arg :: TyFun a1 b1 -> *).
--     SameKind (Apply MapSym0 arg) (MapSym1 arg) =>
--     Data.Singletons.Prelude.Base.MapSym0KindInference

-- type instance Apply MapSym0 l = MapSym1 l

-- type role MapSym1 phantom phantom

-- data MapSym1 (l :: TyFun a b -> Type) (l1 :: TyFun [a] [b])
--   = forall (arg :: [a]).  SameKind (Apply (MapSym1 l) arg) (MapSym2 l arg) => MapSym1KindInference

-- type instance Apply (MapSym1 l1) l2 = Map l1 l2


newtype Matrix ns a = Matrix
  { unMatrix :: MatrixTF ns a
  } deriving anyclass (MonoFoldable)

type instance Element (Matrix ns a) = a

type MatrixTFSym2 (ns :: [Peano]) (t :: Type) = (MatrixTF ns t :: Type)

data MatrixTFSym1 (ns :: [Peano]) (z :: TyFun Type Type)
  = forall (arg :: Type).  SameKind (Apply (MatrixTFSym1 ns) arg) (MatrixTFSym2 ns arg) => MatrixTFSym1KindInference

type instance Apply (MatrixTFSym1 l1) l2 = MatrixTF l1 l2

type role MatrixTFSym0 phantom

data MatrixTFSym0 (l :: TyFun [Peano] (Type ~> Type))
  = forall (arg :: [Peano]).  SameKind (Apply MatrixTFSym0 arg) (MatrixTFSym1 arg) => MatrixTFSym0KindInference

type instance Apply MatrixTFSym0 l = MatrixTFSym1 l

type role MatrixTFSym1 phantom phantom


eqSingMatrix :: forall (peanos :: [Peano]) (a :: Type). Eq a => Sing peanos -> Matrix peanos a -> Matrix peanos a -> Bool
eqSingMatrix = compareSingMatrix (==) True (&&)

ordSingMatrix :: forall (peanos :: [Peano]) (a :: Type). Ord a => Sing peanos -> Matrix peanos a -> Matrix peanos a -> Ordering
ordSingMatrix = compareSingMatrix compare EQ f
  where
    f :: Ordering -> Ordering -> Ordering
    f EQ o = o
    f o _ = o

compareSingMatrix ::
     forall (peanos :: [Peano]) (a :: Type) (c :: Type)
   . (a -> a -> c)
  -> c
  -> (c -> c -> c)
  -> Sing peanos
  -> Matrix peanos a
  -> Matrix peanos a
  -> c
compareSingMatrix f _ _ SNil (Matrix a) (Matrix b) = f a b
compareSingMatrix _ empt _ (SCons SZ _) (Matrix EmptyVec) (Matrix EmptyVec) = empt
compareSingMatrix f empt combine (SCons (SS peanoSingle) moreN) (Matrix (VecCons a moreA)) (Matrix (VecCons b moreB)) =
  combine
    (compareSingMatrix f empt combine moreN a b)
    (compareSingMatrix f empt combine (SCons peanoSingle moreN) (Matrix moreA) (Matrix moreB))

fmapSingMatrix :: forall (peanos :: [Peano]) (a :: Type) (b ::Type). Sing peanos -> (a -> b) -> Matrix peanos a -> Matrix peanos b
fmapSingMatrix SNil f (Matrix a) = Matrix $ f a
fmapSingMatrix (SCons SZ _) _ (Matrix EmptyVec) = Matrix EmptyVec
fmapSingMatrix (SCons (SS peanoSingle) moreN) f (Matrix (VecCons a moreA)) =
  let matA = fmapSingMatrix moreN f a
      matB = fmapSingMatrix (SCons peanoSingle moreN) f (Matrix moreA)
  in consMatrix matA matB

consMatrix :: Matrix ns a -> Matrix (n ': ns) a -> Matrix ('S n ': ns) a
consMatrix papa (Matrix dada) = Matrix $ VecCons papa dada

toListMatrix ::
     forall (peanos :: [Peano]) (a :: Type).
     Sing peanos
  -> Matrix peanos a
  -> [a]
toListMatrix SNil (Matrix a) = [a]
toListMatrix (SCons SZ _) (Matrix EmptyVec) = []
toListMatrix (SCons (SS peanoSingle) moreN) (Matrix (VecCons a moreA)) =
  toListMatrix moreN a <> toListMatrix (SCons peanoSingle moreN) (Matrix moreA)

deriving instance (Eq (MatrixTF ns a)) => Eq (Matrix ns a)

-- instance (Ord a, SingI ns) => Ord (Matrix ns a) where
--   compare :: Matrix ns a -> Matrix ns a -> Ordering
--   compare = ordSingMatrix (sing @_ @ns)

deriving instance (Ord (MatrixTF ns a)) => Ord (Matrix ns a)

-- instance SingI ns => Functor (Matrix ns) where
--   fmap :: (a -> b) -> Matrix ns a -> Matrix ns b
--   fmap = fmapSingMatrix (sing @_ @ns)

instance SingI ns => Functor (Matrix ns) where
  fmap :: (a -> b) -> Matrix ns a -> Matrix ns b
  fmap = fmapSingMatrix (sing @_ @ns)

instance SingI ns => Data.Foldable.Foldable (Matrix ns) where
  foldr :: (a -> b -> b) -> b -> Matrix ns a -> b
  foldr comb b = Data.Foldable.foldr comb b . toListMatrix (sing @_ @ns)

  toList :: Matrix ns a -> [a]
  toList = toListMatrix (sing @_ @ns)

instance Num a => Num (Matrix '[] a) where
  fromInteger :: Integer -> Matrix '[] a
  fromInteger = Matrix . fromInteger
