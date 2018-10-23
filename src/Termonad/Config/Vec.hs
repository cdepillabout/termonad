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

import Data.Distributive (Distributive(distribute))
import qualified Data.Foldable as Data.Foldable
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Kind (Type)
import Data.Singletons.Prelude
import Data.Singletons.TH
import Text.Show (showParen, showString)
import Unsafe.Coerce (unsafeCoerce)

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

--------------------------
-- Misc VecT Operations --
--------------------------

-- take' :: IFin ('S n) m -> VecT n f a -> VecT m f a
-- take' = \case
--   IFS n -> onTail (take' n) \\ n
--   IFZ   -> const EmptyV

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

finToInt :: Fin n -> Int
finToInt FZ = 0
finToInt (FS x) = succ $ finToInt x

-- data instance Sing z where
--   SNil :: Sing '[]
--   SCons :: forall a (n1 :: a) (n2 :: [a]).
--            (Sing n1) -> (Sing n2) -> Sing (n1 : n2)

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

ifinToFin :: IFin n m -> Fin n
ifinToFin IFZ = FZ
ifinToFin (IFS n) = FS (ifinToFin n)

ifinToInt :: IFin n m -> Int
ifinToInt = finToInt . ifinToFin

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

----------
-- Prod --
----------

data Prod :: [Type] -> Type where
  EmptyProd :: Prod '[]
  ProdCons :: forall (a :: Type) (as :: [Type]). !a -> !(Prod as) -> Prod (a ': as)

-- | Infix operator for 'ProdCons.
pattern (:<<) :: (a :: Type) -> Prod as -> Prod (a ': as)
pattern a :<< prod = ProdCons a prod
infixr 6 :<<

curry' :: l ~ (a ': as) => (Prod l -> r) -> a -> Prod as -> r
curry' f a as = f $ ProdCons a as

-----------
-- HList --
-----------

data HList :: (k -> Type) -> [k] -> Type where
  EmptyHList :: HList f '[]
  HListCons :: forall (f :: k -> Type) (a :: k) (as :: [k]). f a -> HList f as -> HList f (a ': as)

-- | Infix operator for 'HListCons'.
pattern (:<) :: (f a :: Type) -> HList f as -> HList f (a ': as)
pattern fa :< hlist = HListCons fa hlist
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

instance MonoFunctor (Vec n a)

instance SingI n => MonoPointed (Vec n a)

instance SingI n => Applicative (Vec n) where
  pure a = vrep_ a

  (<*>) = vap ($)

instance SingI n => Distributive (Vec n) where
  distribute :: Functor f => f (Vec n a) -> Vec n (f a)
  distribute = distributeRep

instance SingI n => Representable (Vec n) where
  type Rep (Vec n) = Fin n

  tabulate :: (Fin n -> a) -> Vec n a
  tabulate = vgen_

  index :: Vec n a -> Fin n -> a
  index = flip vindex

type instance Element (Vec n a) = a

-- | Infix operator for 'VecCons'.
pattern (:*) :: (a :: Type) -> Vec n a -> Vec ('S n) a
pattern a :* vec = VecCons a vec
infixr 6 :*

vgen_ :: SingI n => (Fin n -> a) -> Vec n a
vgen_ = vgen sing

vgen :: SPeano n -> (Fin n -> a) -> Vec n a
vgen SZ _ = EmptyVec
vgen (SS n) f = f FZ :* vgen n (f . FS)

vindex :: Fin n -> Vec n a -> a
vindex FZ (VecCons a _) = a
vindex (FS n) (VecCons _ vec) = vindex n vec

vecSingleton :: a -> Vec N1 a
vecSingleton a = VecCons a EmptyVec

vrep :: Sing n -> a -> Vec n a
vrep SZ _ = EmptyVec
vrep (SS n) a = VecCons a $ vrep n a

imap :: forall n a b. (Fin n -> a -> b) -> Vec n a -> Vec n b
imap _ EmptyVec = EmptyVec
imap f (VecCons a as) = VecCons (f FZ a) (imap (\fin vec -> f (FS fin) vec) as)

vrep_ :: SingI n => a -> Vec n a
vrep_ = vrep sing

vap :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vap _ EmptyVec _ = EmptyVec
vap f (VecCons a as) (VecCons b bs) = VecCons (f a b) $ vap f as bs

vonHead :: (a -> a) -> Vec ('S n) a -> Vec ('S n) a
vonHead f (VecCons a as) = VecCons (f a) as

vdrop :: Sing m -> Vec (m + n) a -> Vec n a
vdrop SZ vec = vec
vdrop (SS n) (VecCons _ vec) = vdrop n vec

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
compareSingMatrix f empt combine (SCons (SS peanoSingle) moreN) (Matrix (VecCons a moreA)) (Matrix (VecCons b moreB)) =
  combine
    (compareSingMatrix f empt combine moreN (Matrix a) (Matrix b))
    (compareSingMatrix f empt combine (SCons peanoSingle moreN) (Matrix moreA) (Matrix moreB))

fmapSingMatrix :: forall (peanos :: [Peano]) (a :: Type) (b ::Type). Sing peanos -> (a -> b) -> Matrix peanos a -> Matrix peanos b
fmapSingMatrix SNil f (Matrix a) = Matrix $ f a
fmapSingMatrix (SCons SZ _) _ (Matrix EmptyVec) = Matrix EmptyVec
fmapSingMatrix (SCons (SS peanoSingle) moreN) f (Matrix (VecCons a moreA)) =
  let matA = fmapSingMatrix moreN f (Matrix a)
      matB = fmapSingMatrix (SCons peanoSingle moreN) f (Matrix moreA)
  in consMatrix matA matB

consMatrix :: Matrix ns a -> Matrix (n ': ns) a -> Matrix ('S n ': ns) a
consMatrix (Matrix a) (Matrix as) = Matrix $ VecCons a as

toListMatrix ::
     forall (peanos :: [Peano]) (a :: Type).
     Sing peanos
  -> Matrix peanos a
  -> [a]
toListMatrix SNil (Matrix a) = [a]
toListMatrix (SCons SZ _) (Matrix EmptyVec) = []
toListMatrix (SCons (SS peanoSingle) moreN) (Matrix (VecCons a moreA)) =
  toListMatrix moreN (Matrix a) <> toListMatrix (SCons peanoSingle moreN) (Matrix moreA)

type family AllT (f :: Peano -> t) (ns :: [Peano]) :: [t] where
  AllT f '[] = '[]
  AllT f (n ': ns) = f n ': AllT f ns

mgen :: forall (ns :: [Peano]) (a :: Type). Prod (AllT SPeano ns) -> (Prod (AllT Fin ns) -> a) -> Matrix ns a
mgen EmptyProd f = Matrix $ unsafeCoerce $ f $ unsafeCoerce EmptyProd
mgen (ProdCons n ns') f =
  Matrix $ unsafeCoerce $ vgen (unsafeCoerce n) $ unMatrix . mgen (unsafeCoerce ns') . curry' (unsafeCoerce f)

myMat :: Matrix '[N2, N3] Int
myMat = mgen (sing :<< sing :<< EmptyProd) f
  where
    f :: Prod '[Fin N2, Fin N3] -> Int
    f (ProdCons f1 (ProdCons f2 EmptyProd)) = finToInt f1 * 3 + finToInt f2

lala :: forall (ns :: [Peano]). Sing ns -> Prod (AllT SPeano ns)
lala SNil = EmptyProd
lala (SCons n ns) = ProdCons n $ lala ns

mgen' :: forall (ns :: [Peano]) (a :: Type). Sing ns -> (Prod (AllT Fin ns) -> a) -> Matrix ns a
mgen' SNil f = Matrix $ f EmptyProd
mgen' (SCons (n :: SPeano foo) (ns' :: Sing oaoa)) f =
  Matrix $ (vgen (n :: SPeano foo) $ (gagaga :: Fin foo -> MatrixTF oaoa a) :: Vec foo (MatrixTF oaoa a))
  where
    gagaga :: Fin foo -> MatrixTF oaoa a
    gagaga faaa = unMatrix $ (mgen' ns' $ byebye faaa :: Matrix oaoa a)

    byebye :: Fin foo -> Prod (AllT Fin oaoa) -> a
    byebye faaa = curry' f faaa

myMat2 :: Matrix '[N2, N3] Int
myMat2 = mgen' sing f
  where
    f :: Prod '[Fin N2, Fin N3] -> Int
    f (ProdCons f1 (ProdCons f2 EmptyProd)) = finToInt f1 * 3 + finToInt f2

mgen'' :: forall (ns :: [Peano]) (a :: Type). Sing ns -> (HList Fin ns -> a) -> Matrix ns a
mgen'' SNil f = Matrix $ f EmptyHList
mgen'' (SCons (n :: SPeano foo) (ns' :: Sing oaoa)) f =
  Matrix $ (vgen n $ (gagaga :: Fin foo -> MatrixTF oaoa a) :: Vec foo (MatrixTF oaoa a))
  where
    gagaga :: Fin foo -> MatrixTF oaoa a
    gagaga faaa = unMatrix $ (mgen'' ns' $ byebye faaa :: Matrix oaoa a)

    byebye :: Fin foo -> HList Fin oaoa -> a
    byebye faaa = f . HListCons faaa

myMat3 :: Matrix '[N2, N3] Int
myMat3 = mgen'' sing f
  where
    f :: HList Fin '[N2, N3] -> Int
    f (HListCons f1 (HListCons f2 EmptyHList)) = finToInt f1 * 3 + finToInt f2

mgen_ :: SingI ns => (HList Fin ns -> a) -> Matrix ns a
mgen_ = mgen'' sing

mindex :: HList Fin ns -> Matrix ns a -> a
mindex EmptyHList (Matrix a) = a
mindex (HListCons i is) (Matrix vec) = mindex is $ Matrix (vindex i vec)

mmap :: forall (ns :: [Peano]) a b. Sing ns -> (HList Fin ns -> a -> b) -> Matrix ns a -> Matrix ns b
mmap SNil f (Matrix a) = Matrix (f EmptyHList a)
mmap (SCons _ ns) f (Matrix vec) =
  Matrix
    (imap
      (\fin -> unMatrix . mmap ns (\hlist -> f (HListCons fin hlist)) . Matrix)
      vec
    )


mmap_ :: SingI ns => (HList Fin ns -> a -> b) -> Matrix ns a -> Matrix ns b
mmap_ = mmap sing

testtest1 :: Matrix '[N2, N3] String
testtest1 = mmap_ f (Matrix ((1 :* 2 :* 3 :* EmptyVec) :* (4 :* 5 :* 6 :* EmptyVec) :* EmptyVec))
  where
    f :: HList Fin '[N2, N3] -> Int -> String
    f (HListCons finA (HListCons finB EmptyHList)) i = show (i, finToInt finA, finToInt finB)

testtest2 :: Matrix '[N2] String
testtest2 = mmap_ f (Matrix (1 :* 2 :* EmptyVec))
  where
    f :: HList Fin '[N2] -> Int -> String
    f (HListCons finA EmptyHList) i = show (i, finToInt finA)

testtest3 :: Matrix '[] String
testtest3 = mmap_ f (Matrix 100)
  where
    f :: HList Fin '[] -> Int -> String
    f EmptyHList i = show i

testtest4 :: Matrix '[N0, N2] String
testtest4 = mmap_ f (Matrix EmptyVec)
  where
    f :: HList Fin '[N0, N2] -> Int -> String
    f (HListCons (finA :: Fin N0) (HListCons (_ :: Fin N2) EmptyHList)) _ =
      case finA of {}

onMatrixTF :: (MatrixTF ns a -> MatrixTF ms b) -> Matrix ns a -> Matrix ms b
onMatrixTF f (Matrix mat) = Matrix $ f mat

onMatrix :: (Matrix ns a -> Matrix ms b) -> MatrixTF ns a -> MatrixTF ms b
onMatrix f = unMatrix . f . Matrix

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
  tabulate = mgen_

  index :: Matrix ns a -> HList Fin ns -> a
  index = flip mindex

instance Num a => Num (Matrix '[] a) where
  Matrix a + Matrix b = Matrix (a + b)

  Matrix a * Matrix b = Matrix (a * b)

  Matrix a - Matrix b = Matrix (a - b)

  abs (Matrix a) = Matrix (abs a)

  signum (Matrix a) = Matrix (signum a)

  fromInteger :: Integer -> Matrix '[] a
  fromInteger = Matrix . fromInteger
