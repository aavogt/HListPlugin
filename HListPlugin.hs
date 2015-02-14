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
       seen :: IORef (M.Map TcTyVar [[Xi]]),
       sameLength :: TyCon }

ini = do
  Found _loc hlist <- findImportedModule (mkModuleName "HList") Nothing


  hlength <- tcLookupTyCon =<< lookupOrig hlist (mkTcOcc "HLength")
  sameLengthName <- lookupOrig hlist (mkTcOcc "SameLength")
  sameLengthTC <- tcLookupTyCon sameLengthName

  seenRef <- tcPluginIO (newIORef M.empty)
  return S { isHLength = (==) hlength,
             seen = seenRef,
             sameLength = sameLengthTC }

stop _ = return ()



-- 1. pretend that HList is representational
--
-- 2. Given (HLength x ~ fsk1, HLength y ~ fsk2, fsk1 ~ fsk2) add the
--    constraint (SameLength x y)
sol state given derived wanted = do

    seenMap <- tcPluginIO (readIORef (seen state))
    ev <- newFlexiTyVar anyKind
    (_, env) <- getEnvs

    let state' :: M.Map TcTyVar [[Xi]]
        state' = M.fromListWith (++) [ (fsk, [xis])
                | CFunEqCan { cc_fun = f, cc_tyargs = xis, cc_fsk = fsk } <- given,
                  isHLength state f  ]
                    M.\\ seenMap
        newReq = concat [ map CNonCanonical $ map (opts !!) [1] -- just Wanted

                                        



                    | CTyEqCan { cc_tyvar = fsk, cc_rhs = TyVarTy rhs } <- given,

                    Just [[kL,xiL]] <- [M.lookup fsk state'],
                    Just [[kR,xiR]] <- [M.lookup rhs state'],
                    let predType = TyConApp (sameLength state) [kL,kR,xiL,xiR],
                    let dummyLoc = CtLoc AppOrigin env initialSubGoalDepth,

                    let opts = [ CtGiven
                                  predType
                                  (EvId ev)
                                  dummyLoc,
                                CtWanted predType
                                    ev
                                    dummyLoc,
                                CtDerived predType dummyLoc
                            ]
                    ]
    tcPluginIO (writeIORef (seen state) (state' `M.union` seenMap))
    tcPluginTrace "XXX" (ppr newReq)
    return (TcPluginOk [] newReq)
