{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fplugin=HListPlugin #-}
module Main where

import HList
import Data.Coerce

newtype C = C Char deriving Eq

main = print
    [coerce HNil == HNil,
     coerce (C 'a' `HCons` HNil) == 'a' `HCons` HNil]



-- should be
-- q :: Coercible b C => HList '[C] -> HList '[b]
q = coerce (C 'a' `HCons` HNil)
