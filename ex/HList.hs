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

type family HLength (x :: [k]) :: HNat
type instance HLength '[] = HZero
type instance HLength (x ': xs) = HSucc (HLength xs)

data HNat = HZero | HSucc HNat

class (SameLength' x y, SameLength' y x) =>
        SameLength (x :: [k]) (y :: [m])


class SameLength' (es1 :: [k]) (es2 :: [m])
instance (es2 ~ '[]) => SameLength' '[] es2
instance (SameLength' xs ys, es2 ~ (y ': ys)) => SameLength' (x ': xs) es2

