{-# LANGUAGE ViewPatterns #-}
module HListPlugin (plugin) where

import Control.Applicative ((<$>))
import GhcPlugins
import TcRnTypes
import TcEvidence
import TcType
import TcPluginM
import qualified Data.Map as M
import TypeRep
import Data.IORef
import TypeRep
import Data.Maybe

plugin = defaultPlugin { tcPlugin =
  \ _args -> Just (TcPlugin ini sol stop)
  }


data S = S {
       isHLength :: TyCon -> Bool,
       sameLength, mapCoerce, hNat, hlist :: TyCon }

ini = do
  Found _loc hlist <- findImportedModule (mkModuleName "HList") Nothing
  let hlistTC :: String -> TcPluginM TyCon
      hlistTC str = tcLookupTyCon =<< lookupOrig hlist (mkTcOcc str)

  hlength <- hlistTC "HLength"
  sameLength <- hlistTC "SameLength"
  hNat <- hlistTC "HNat"
  hlist <- hlistTC "HList"
  mapCoerce <- hlistTC "MapCoerce"

  seenRef <- tcPluginIO (newIORef M.empty)
  return S { isHLength = (==) hlength,
             sameLength = sameLength,
             mapCoerce = mapCoerce,
             hNat = hNat,
             hlist = hlist  }

stop _ = return ()

sol state givens deriveds wanteds = do
 tcPluginTrace "WANTEDS" (ppr wanteds)
 tcPluginTrace "GIVENS" (ppr givens)
 tcPluginTrace "DERIVEDS" (ppr deriveds)
 let  isHNat :: Type -> Bool
      isHNat x = TyConApp (hNat state) [] == x

      unHLength :: Type -> Maybe (Kind, Type)
      unHLength (TyConApp con [k, x]) | isHLength state con = Just (k,x)
      unHLength _ = Nothing

      sl :: (Kind,Type) -> (Kind, Type) -> Type
      sl (kl,l) (kr, r) = TyConApp (sameLength state) [kl, kr, l, r]

      hListTyCon :: TyCon
      hListTyCon = hlist state



      toResult :: [([(EvTerm, Ct)] , TcPluginM [Ct])] -> TcPluginM TcPluginResult
      toResult (unzip -> (discharged, new)) =
        TcPluginOk (concat discharged) . concat <$> sequence new

  in toResult [ case pred of
    TyConApp ((==) eqTyCon -> True)
        [ isHNat -> True,
          unHLength -> Just kll,
          unHLength -> Just krr]
                  -> ([(EvId var, want)],
                         -- discharge the original constraint (possibly
                         -- don't do this with a more general version of
                         -- this plugin, since we might add constraints
                         -- that are weaker but still help (for example
                         -- SameLength' x y)

                         -- add the SameLength constraint
                        return [wantedNC (sl kll krr)])

    {- When we need a

       Coercible (r xs) (HList xs')

      Instead be satisfied by:

       (MapCoerce xs xs',
        Coercible r HList)
    -}
    TyConApp ((==) coercibleTyCon -> True)
        [ (==) liftedTypeKind -> True, -- is *
          rxs,
          TyConApp ((==) hListTyCon -> True) [xs'] ] ->
            ([(EvId var, want)], do
                let -- [*] -> *
                    containerKind = mkArrowKind listKind liftedTypeKind

                    -- [*]
                    listKind = TyConApp listTyCon [liftedTypeKind]

                xs <- newFlexiTyVar listKind
                r <- newFlexiTyVar containerKind

                return [
                  -- rxs ~ (r xs)
                  wantedNC (TyConApp eqTyCon [liftedTypeKind, rxs, TyVarTy r `AppTy` TyVarTy xs]),

                  -- MapCoerce xs xs'
                  wantedNC (TyConApp (mapCoerce state) [TyVarTy xs, xs']),

                  -- Coerce r HList
                  -- (since r might be a newtype of HList like Record or TIP)
                  wantedNC (TyConApp coercibleTyCon [containerKind, TyVarTy r, TyConApp hListTyCon []])
                  ])
    _ -> ([], return [])


     | want <- wanteds,
       CNonCanonical
          (CtWanted
              pred
              var
              loc) <- [want],
       let wantedNC ty = CNonCanonical (CtWanted ty var loc)
       ]
