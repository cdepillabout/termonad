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
  -- , genMatrix_
  -- , setSubmatrix
  -- , genVec_
  -- , vSetAt'
  -- )
    where

import Termonad.Prelude hiding ((\\), index)

import Data.Distributive (Distributive(distribute))
import qualified Data.Foldable as Data.Foldable
import Data.Functor.Rep (Representable(..), apRep, bindRep, distributeRep, pureRep)
import Data.Kind (Type)
import Data.Singletons.Prelude
import Data.Singletons.TH
import Text.Show (showParen, showString)

--------------------------
-- Misc VecT Operations --
--------------------------

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
--   => HList (Uncur3 Range) nlms -> (HList Fin ms -> a -> a) -> M ns a -> M ns a
-- updateSubmatrix = \case
--   Ø              -> \f -> (f Ø <$>)
--   Uncur3 r :< rs -> \f -> onMatrix . updateRange r $ \i ->
--     asM . updateSubmatrix rs $ f . (i :<)

-- setSubmatrix
--   :: (ns ~ Fsts3 nlms, ms ~ Thds3 nlms)
--   => HList (Uncur3 Range) nlms -> M ms a -> M ns a -> M ns a
-- setSubmatrix rs sm = updateSubmatrix rs $ \is _ -> indexMatrix is sm


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

toIntFin :: Fin n -> Int
toIntFin FZ = 0
toIntFin (FS x) = succ $ toIntFin x

data instance Sing (z :: Fin n) where
  SFZ :: Sing 'FZ
  SFS :: Sing x -> Sing ('FS x)

instance SingI 'FZ where
  sing = SFZ

instance SingI n => SingI ('FS n) where
  sing = SFS sing

instance SingKind (Fin n) where
  type Demote (Fin n) = Fin n
  fromSing :: forall (a :: Fin n). Sing a -> Fin n
  fromSing SFZ = FZ
  fromSing (SFS fin) = FS (fromSing fin)

  toSing :: Fin n -> SomeSing (Fin n)
  toSing FZ = SomeSing SFZ
  toSing (FS fin) =
    case toSing fin of
      SomeSing n -> SomeSing (SFS n)

instance Show (Sing 'FZ) where
  show SFZ = "SFZ"

instance Show (Sing n) => Show (Sing ('FS n)) where
  showsPrec d (SFS n) =
    showParen (d > 10) $
    showString "SFS " . showsPrec 11 n

----------
-- IFin --
----------

data IFin :: Peano -> Peano -> Type where
  IFZ :: forall (n :: Peano). IFin ('S n) 'Z
  IFS :: forall (n :: Peano) (m :: Peano). !(IFin n m) -> IFin ('S n) ('S m)

deriving instance Eq   (IFin x y)
deriving instance Ord  (IFin x y)
deriving instance Show (IFin x y)

toFinIFin :: IFin n m -> Fin n
toFinIFin IFZ = FZ
toFinIFin (IFS n) = FS (toFinIFin n)

toIntIFin :: IFin n m -> Int
toIntIFin = toIntFin . toFinIFin

data instance Sing (z :: IFin n m) where
  SIFZ :: Sing 'IFZ
  SIFS :: Sing x -> Sing ('IFS x)

instance SingI 'IFZ where
  sing = SIFZ

instance SingI n => SingI ('IFS n) where
  sing = SIFS sing

instance SingKind (IFin n m) where
  type Demote (IFin n m) = IFin n m
  fromSing :: forall (a :: IFin n m). Sing a -> IFin n m
  fromSing SIFZ = IFZ
  fromSing (SIFS fin) = IFS (fromSing fin)

  toSing :: IFin n m -> SomeSing (IFin n m)
  toSing IFZ = SomeSing SIFZ
  toSing (IFS fin) =
    case toSing fin of
      SomeSing n -> SomeSing (SIFS n)

instance Show (Sing 'IFZ) where
  show SIFZ = "SIFZ"

instance Show (Sing n) => Show (Sing ('IFS n)) where
  showsPrec d (SIFS n) =
    showParen (d > 10) $
    showString "SIFS " . showsPrec 11 n

-----------
-- HList --
-----------

data HList :: (k -> Type) -> [k] -> Type where
  EmptyHList :: HList f '[]
  (:<) :: forall (f :: k -> Type) (a :: k) (as :: [k]). f a -> HList f as -> HList f (a ': as)

infixr 6 :<

-- | Data constructor for ':<'.
pattern ConsHList :: (f a :: Type) -> HList f as -> HList f (a ': as)
pattern ConsHList fa hlist = fa :< hlist

---------
-- Vec --
---------

data Vec (n :: Peano) :: Type -> Type where
  EmptyVec :: Vec 'Z a
  (:*) :: !a -> !(Vec n a) -> Vec ('S n) a
  deriving anyclass MonoFoldable

infixr 6 :*

-- | Data constructor for ':*'.
pattern ConsVec :: (a :: Type) -> Vec n a -> Vec ('S n) a
pattern ConsVec a vec = a :* vec

deriving instance Eq a => Eq (Vec n a)
deriving instance Ord a => Ord (Vec n a)
deriving instance Show a => Show (Vec n a)

deriving instance Functor (Vec n)
deriving instance Foldable (Vec n)

instance MonoFunctor (Vec n a)

instance SingI n => MonoPointed (Vec n a)

instance SingI n => Applicative (Vec n) where
  pure a = replaceVec_ a

  (<*>) = apVec ($)

instance SingI n => Distributive (Vec n) where
  distribute :: Functor f => f (Vec n a) -> Vec n (f a)
  distribute = distributeRep

instance SingI n => Representable (Vec n) where
  type Rep (Vec n) = Fin n

  tabulate :: (Fin n -> a) -> Vec n a
  tabulate = genVec_

  index :: Vec n a -> Fin n -> a
  index = flip indexVec

instance SingI n => Monad (Vec n) where
  (>>=) :: Vec n a -> (a -> Vec n b) -> Vec n b
  (>>=) = bindRep

type instance Element (Vec n a) = a

genVec_ :: SingI n => (Fin n -> a) -> Vec n a
genVec_ = genVec sing

genVec :: SPeano n -> (Fin n -> a) -> Vec n a
genVec SZ _ = EmptyVec
genVec (SS n) f = f FZ :* genVec n (f . FS)

indexVec :: Fin n -> Vec n a -> a
indexVec FZ (a :* _) = a
indexVec (FS n) (_ :* vec) = indexVec n vec

singletonVec :: a -> Vec N1 a
singletonVec a = ConsVec a EmptyVec

replaceVec :: Sing n -> a -> Vec n a
replaceVec SZ _ = EmptyVec
replaceVec (SS n) a = a :* replaceVec n a

imapVec :: forall n a b. (Fin n -> a -> b) -> Vec n a -> Vec n b
imapVec _ EmptyVec = EmptyVec
imapVec f (a :* as) = f FZ a :* imapVec (\fin vec -> f (FS fin) vec) as

replaceVec_ :: SingI n => a -> Vec n a
replaceVec_ = replaceVec sing

apVec :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
apVec _ EmptyVec _ = EmptyVec
apVec f (a :* as) (b :* bs) = f a b :* apVec f as bs

onHeadVec :: (a -> a) -> Vec ('S n) a -> Vec ('S n) a
onHeadVec f (a :* as) = f a :* as

dropVec :: Sing m -> Vec (m + n) a -> Vec n a
dropVec SZ vec = vec
dropVec (SS n) (_ :* vec) = dropVec n vec

takeVec :: IFin n m -> Vec n a -> Vec m a
takeVec IFZ _ = EmptyVec
takeVec (IFS n) (a :* vec) = a :* takeVec n vec

updateAtVec :: Fin n -> (a -> a) -> Vec n a -> Vec n a
updateAtVec FZ f (a :* vec)  = f a :* vec
updateAtVec (FS n) f (a :* vec)  = a :* updateAtVec n f vec

setAtVec :: Fin n -> a -> Vec n a -> Vec n a
setAtVec fin a = updateAtVec fin (const a)

------------
-- Matrix --
------------

type family MatrixTF (ns :: [Peano]) (a :: Type) :: Type where
  MatrixTF '[] a = a
  MatrixTF (n ': ns) a = Vec n (MatrixTF ns a)

newtype Matrix ns a = Matrix
  { unMatrix :: MatrixTF ns a
  }
  deriving anyclass (MonoFoldable)

type instance Element (Matrix ns a) = a

---------------------------------
-- Defunctionalization Symbols --
---------------------------------

type MatrixTFSym2 (ns :: [Peano]) (t :: Type) = (MatrixTF ns t :: Type)

data MatrixTFSym1 (ns :: [Peano]) (z :: TyFun Type Type)
  = forall (arg :: Type).  SameKind (Apply (MatrixTFSym1 ns) arg) (MatrixTFSym2 ns arg) => MatrixTFSym1KindInference

type instance Apply (MatrixTFSym1 l1) l2 = MatrixTF l1 l2

type role MatrixTFSym0 phantom

data MatrixTFSym0 (l :: TyFun [Peano] (Type ~> Type))
  = forall (arg :: [Peano]).  SameKind (Apply MatrixTFSym0 arg) (MatrixTFSym1 arg) => MatrixTFSym0KindInference

type instance Apply MatrixTFSym0 l = MatrixTFSym1 l

type role MatrixTFSym1 phantom phantom

----------------------
-- Matrix Functions --
----------------------

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
compareSingMatrix f empt combine (SCons (SS peanoSingle) moreN) (Matrix (a :* moreA)) (Matrix (b :* moreB)) =
  combine
    (compareSingMatrix f empt combine moreN (Matrix a) (Matrix b))
    (compareSingMatrix f empt combine (SCons peanoSingle moreN) (Matrix moreA) (Matrix moreB))

fmapSingMatrix :: forall (peanos :: [Peano]) (a :: Type) (b ::Type). Sing peanos -> (a -> b) -> Matrix peanos a -> Matrix peanos b
fmapSingMatrix SNil f (Matrix a) = Matrix $ f a
fmapSingMatrix (SCons SZ _) _ (Matrix EmptyVec) = Matrix EmptyVec
fmapSingMatrix (SCons (SS peanoSingle) moreN) f (Matrix (a :* moreA)) =
  let matA = fmapSingMatrix moreN f (Matrix a)
      matB = fmapSingMatrix (SCons peanoSingle moreN) f (Matrix moreA)
  in consMatrix matA matB

consMatrix :: Matrix ns a -> Matrix (n ': ns) a -> Matrix ('S n ': ns) a
consMatrix (Matrix a) (Matrix as) = Matrix $ ConsVec a as

toListMatrix ::
     forall (peanos :: [Peano]) (a :: Type).
     Sing peanos
  -> Matrix peanos a
  -> [a]
toListMatrix SNil (Matrix a) = [a]
toListMatrix (SCons SZ _) (Matrix EmptyVec) = []
toListMatrix (SCons (SS peanoSingle) moreN) (Matrix (a :* moreA)) =
  toListMatrix moreN (Matrix a) <> toListMatrix (SCons peanoSingle moreN) (Matrix moreA)

genMatrix ::
     forall (ns :: [Peano]) (a :: Type).
     Sing ns
  -> (HList Fin ns -> a)
  -> Matrix ns a
genMatrix SNil f = Matrix $ f EmptyHList
genMatrix (SCons (n :: SPeano foo) (ns' :: Sing oaoa)) f =
  Matrix $ (genVec n $ (gagaga :: Fin foo -> MatrixTF oaoa a) :: Vec foo (MatrixTF oaoa a))
  where
    gagaga :: Fin foo -> MatrixTF oaoa a
    gagaga faaa = unMatrix $ (genMatrix ns' $ byebye faaa :: Matrix oaoa a)

    byebye :: Fin foo -> HList Fin oaoa -> a
    byebye faaa = f . ConsHList faaa

genMatrix_ :: SingI ns => (HList Fin ns -> a) -> Matrix ns a
genMatrix_ = genMatrix sing

indexMatrix :: HList Fin ns -> Matrix ns a -> a
indexMatrix EmptyHList (Matrix a) = a
indexMatrix (i :< is) (Matrix vec) = indexMatrix is $ Matrix (indexVec i vec)

imapMatrix :: forall (ns :: [Peano]) a b. Sing ns -> (HList Fin ns -> a -> b) -> Matrix ns a -> Matrix ns b
imapMatrix SNil f (Matrix a) = Matrix (f EmptyHList a)
imapMatrix (SCons _ ns) f matrix =
  onMatrixTF
    (imapVec (\fin -> onMatrix (imapMatrix ns (\hlist -> f (ConsHList fin hlist)))))
    matrix

imapMatrix_ :: SingI ns => (HList Fin ns -> a -> b) -> Matrix ns a -> Matrix ns b
imapMatrix_ = imapMatrix sing

onMatrixTF :: (MatrixTF ns a -> MatrixTF ms b) -> Matrix ns a -> Matrix ms b
onMatrixTF f (Matrix mat) = Matrix $ f mat

onMatrix :: (Matrix ns a -> Matrix ms b) -> MatrixTF ns a -> MatrixTF ms b
onMatrix f = unMatrix . f . Matrix

updateAtMatrix :: HList Fin ns -> (a -> a) -> Matrix ns a -> Matrix ns a
updateAtMatrix EmptyHList _ mat = mat
updateAtMatrix (n :< ns) f mat =
  onMatrixTF (updateAtVec n (onMatrix (updateAtMatrix ns f))) mat

setAtMatrix :: HList Fin ns -> a -> Matrix ns a -> Matrix ns a
setAtMatrix fins a = updateAtMatrix fins (const a)

----------------------
-- Matrix Instances --
----------------------

deriving instance (Eq (MatrixTF ns a)) => Eq (Matrix ns a)

deriving instance (Ord (MatrixTF ns a)) => Ord (Matrix ns a)

deriving instance (Show (MatrixTF ns a)) => Show (Matrix ns a)

instance SingI ns => Functor (Matrix ns) where
  fmap :: (a -> b) -> Matrix ns a -> Matrix ns b
  fmap = fmapSingMatrix (sing @_ @ns)

instance SingI ns => Data.Foldable.Foldable (Matrix ns) where
  foldr :: (a -> b -> b) -> b -> Matrix ns a -> b
  foldr comb b = Data.Foldable.foldr comb b . toListMatrix (sing @_ @ns)

  toList :: Matrix ns a -> [a]
  toList = toListMatrix (sing @_ @ns)

instance SingI ns => Distributive (Matrix ns) where
  distribute :: Functor f => f (Matrix ns a) -> Matrix ns (f a)
  distribute = distributeRep

instance SingI ns => Representable (Matrix ns) where
  type Rep (Matrix ns) = HList Fin ns

  tabulate :: (HList Fin ns -> a) -> Matrix ns a
  tabulate = genMatrix_

  index :: Matrix ns a -> HList Fin ns -> a
  index = flip indexMatrix

instance Num a => Num (Matrix '[] a) where
  Matrix a + Matrix b = Matrix (a + b)

  Matrix a * Matrix b = Matrix (a * b)

  Matrix a - Matrix b = Matrix (a - b)

  abs (Matrix a) = Matrix (abs a)

  signum (Matrix a) = Matrix (signum a)

  fromInteger :: Integer -> Matrix '[] a
  fromInteger = Matrix . fromInteger

instance SingI ns => Applicative (Matrix ns) where
  pure :: a -> Matrix ns a
  pure = pureRep

  (<*>) :: Matrix ns (a -> b) -> Matrix ns a -> Matrix ns b
  (<*>) = apRep

instance SingI ns => Monad (Matrix ns) where
  (>>=) :: Matrix ns a -> (a -> Matrix ns b) -> Matrix ns b
  (>>=) = bindRep
