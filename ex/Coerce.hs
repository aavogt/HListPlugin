{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fplugin=HListPlugin #-}
module Main where

import HList
import Data.Coerce

newtype C = C Char deriving Eq


main = print
    [coerce HNil == HNil

     -- This one doesn't work: ghc does not seem to call the plugin
     -- before deciding that there is no coercion.
     -- What's different when I inline q?
     -- , coerce (C 'a' `HCons` HNil) == ('a' `HCons` HNil)

     , q == 'a' `HCons` HNil
     , q == Record ('a' `HCons` HNil)
     ]

-- should be (and is) a type error.
-- bad = q `asTypeOf` (Proxy :: Proxy '[C])


-- should be
-- q :: (Coercible t HList, Coercible b C) => t '[b]
q = coerce (C 'a' `HCons` HNil)
