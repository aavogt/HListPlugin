{-# LANGUAGE ViewPatterns #-}
module HListPlugin (plugin) where

import GhcPlugins
import TcRnTypes
import TcEvidence
import TcType
import TcPluginM
import qualified Data.Map as M
import TypeRep
import Data.IORef
import TypeRep

plugin = defaultPlugin { tcPlugin =
  \ _args -> Just (TcPlugin ini sol stop)
  }


data S = S {
       isHLength :: TyCon -> Bool,
       sameLength, hNat :: TyCon }

ini = do
  Found _loc hlist <- findImportedModule (mkModuleName "HList") Nothing
  let hlistTC :: String -> TcPluginM TyCon
      hlistTC str = tcLookupTyCon =<< lookupOrig hlist (mkTcOcc str)

  hlength <- hlistTC "HLength"
  sameLength <- hlistTC "SameLength"
  hNat <- hlistTC "HNat"

  seenRef <- tcPluginIO (newIORef M.empty)
  return S { isHLength = (==) hlength,
             sameLength = sameLength,
             hNat = hNat }

stop _ = return ()

sol state _given _derived wanteds = return $ uncurry TcPluginOk $ unzip $
  let isHNat :: Type -> Bool
      isHNat x = TyConApp (hNat state) [] == x

      unHLength :: Type -> Maybe (Kind, Type)
      unHLength (TyConApp con [k, x]) | isHLength state con = Just (k,x)
      unHLength _ = Nothing

      sl :: (Kind,Type) -> (Kind, Type) -> Type
      sl (kl,l) (kr, r) = TyConApp (sameLength state) [kl, kr, l, r]

  in [ ((EvId var, want),
       -- discharge the original constraint (possibly don't do this
       -- with a more general version of this plugin, since we might
       -- add constraints that are weaker but still help
       -- (for example SameLength' x y)

       -- add the SameLength constraint
       CNonCanonical (CtWanted (sl kll krr) var loc))
     | want <- wanteds,
       CNonCanonical
          (CtWanted
              (TyConApp ((==) eqTyCon -> True)
                  [ isHNat -> True,
                    unHLength -> Just kll,
                    unHLength -> Just krr])
              var
              loc) <- [want]
       ]
