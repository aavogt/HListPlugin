{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin=HListPlugin #-}
module Main where
import HList


main = return ()

p = Proxy :: HLength (x :: [*]) ~ HLength (y :: [*]) => Proxy '(y, x)


q = p `asTypeOf` (Proxy :: Proxy '( [(),()], (t :: [*])))
