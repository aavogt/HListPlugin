{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- a subset of HList definitions needed
module HList (module HList, module Data.Proxy) where
import Data.Proxy
import Data.Coerce (Coercible)
import GHC.Exts (Constraint)

newtype Record xs = Record (HList xs)

deriving instance Eq (HList xs) => Eq (Record xs)

data family HList (l::[*])
data instance HList '[] = HNil
data instance HList (x ': xs) = x `HCons` HList xs

deriving instance Eq (HList '[])
deriving instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs))

deriving instance Show (HList '[])
deriving instance (Show x, Show (HList xs)) => Show (HList (x ': xs))

type MapCoerce (xs :: [*]) (ys :: [*]) = (SameLength xs ys, MapCoerce' xs ys)

type family MapCoerce' (xs :: [*]) (ys :: [*]) :: Constraint
type instance MapCoerce' '[] '[] = ()
type instance MapCoerce' (x ': xs) (y ': ys) = (Coercible x y, MapCoerce' xs ys)

type family HLength (x :: [k]) :: HNat
type instance HLength '[] = HZero
type instance HLength (x ': xs) = HSucc (HLength xs)

data HNat = HZero | HSucc HNat

type SameLength x y = (SameLength' x y, SameLength' y x)

class SameLength' (es1 :: [k]) (es2 :: [m])
instance (es2 ~ '[]) => SameLength' '[] es2
instance (SameLength' xs ys, es2 ~ (y ': ys)) => SameLength' (x ': xs) es2

