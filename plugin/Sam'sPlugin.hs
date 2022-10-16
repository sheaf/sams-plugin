{-# LANGUAGE ScopedTypeVariables #-}

module Sam'sPlugin ( plugin ) where

-- base
import Control.Monad
  ( zipWithM )
import Data.Coerce
  ( coerce )
import Data.Monoid
  ( Ap(..) )
import Data.IORef
  ( IORef, atomicModifyIORef, newIORef, readIORef )
import System.IO.Unsafe
  ( unsafePerformIO )
 
-- transformers
import Control.Monad.IO.Class
  ( liftIO )

-- ghc
import GHC.Builtin.Types
  ( typeSymbolKind )
import GHC.Core
  ( Alt(..), AltCon(..) )
import GHC.Core.DataCon
  ( FieldLabel(..), dataConFieldLabels, dataConOrigArgTys )
import GHC.Core.InstEnv
  ( ClsInst(..) )
import GHC.Core.Make
  ( mkCoreConApps, mkCoreLams, mkWildCase )
import GHC.Core.TyCo.Rep
  ( Scaled(..), Type(..), TyLit(StrTyLit), RuntimeRepType )
import GHC.Core.TyCon
  ( isDataTyCon, tyConBinders, tyConDataCons, tyConResKind )
import GHC.Core.Type
  ( getRuntimeRep
  , substTyWith, tyCoVarsOfTypesWellScoped
  , typeKind )
import GHC.Data.Bag
  ( unitBag, unionBags, listToBag )
import GHC.Data.Maybe
  ( orElse )
import qualified GHC.Data.Strict as Strict
  ( Maybe(..) )
import GHC.Hs.Extension
  ( GhcTc )
import GHC.Hs.Expr
  ( HsExpr(..) )
import GHC.Hs.Utils
  ( mkHsWrap )
import GHC.Parser.Annotation
  ( noLocA )
import qualified GHC.Plugins as GHC
  ( Plugin(..), defaultPlugin, purePlugin )
import GHC.Tc.Types
  ( TcM, TcGblEnv(..), getLclEnvLoc )
import GHC.Tc.Types.Constraint
  ( ctLocEnv )
import GHC.Tc.Types.Evidence
  ( HsWrapper(..), TcEvBinds(..), EvBind(..) )
import GHC.Tc.Types.Origin
  ( CtOrigin(..) )
import GHC.Tc.Utils.Env as GHC
  ( newDFunName )
import GHC.Tc.Utils.Instantiate
  ( newClsInst )
import GHC.Tc.Utils.Monad
  ( traceTc )
import GHC.Types.Id
  ( idType, mkSysLocalM, localiseId )
import GHC.Types.SrcLoc
  ( SrcSpan(..) )
import GHC.Types.Unique.Supply
  ( getUniquesM )
import GHC.Types.Var
  ( binderVars, setVarUnique )
import GHC.Unit.Module
  ( getModule )
import GHC.Unit.Module.Env
  ( ModuleEnv, emptyModuleEnv
  , extendModuleEnvWith, lookupModuleEnv )
import GHC.Utils.Outputable
  ( (<+>), braces, text, vcat )
import Language.Haskell.Syntax.Basic
  ( FieldLabelString(field_label) )
import Language.Haskell.Syntax.Binds
  ( LHsBind, HsBindLR(..) )
import Language.Haskell.Syntax.Extension
  ( noExtField )

-- ghc-tcplugin-api
import GHC.TcPlugin.API
import GHC.TcPlugin.API.Internal
  ( unsafeLiftTcM )

--------------------------------------------------------------------------------
-- Plugin definition and setup/finalisation.

plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { GHC.tcPlugin              = \ _args -> Just $ mkTcPlugin sam'sTcPlugin
    , GHC.typeCheckResultAction = \_args _mod_summary tc_gbl_env ->
                                  sam'sTcGblEnvPlugin tc_gbl_env
    , GHC.pluginRecompile       = GHC.purePlugin
    }

sam'sTcPlugin :: TcPlugin
sam'sTcPlugin =
  TcPlugin
    { tcPluginInit    = sam'sTcPluginInit
    , tcPluginSolve   = sam'sTcPluginSolve
    , tcPluginRewrite = \ _ -> emptyUFM
    , tcPluginStop    = \ _ -> return ()
    }

data Sam'sPluginDefs =
  Sam'sPluginDefs
    { sam'sOpticsModule   :: !Module
    , sam'sGetFieldClass  :: !Class
    , sam'sSetFieldClass  :: !Class
    , sam'sOpticsClass    :: !Class
    , genSam'sOpticsClass :: !Class
    }

findModule :: MonadTcPlugin m => String -> m Module
findModule modName = do
  findResult <- findImportedModule ( mkModuleName modName ) NoPkgQual
  case findResult of
    Found _ res ->
      pure res
    FoundMultiple _ ->
      error $ "Sam'sPlugin: found multiple modules named " <> modName <> "."
    _ ->
      error $ "Sam'sPlugin: could not find any module named " <> modName <> "."

sam'sTcPluginInit :: TcPluginM Init Sam'sPluginDefs
sam'sTcPluginInit = do
  sam'sOpticsModule <- findModule "Sam'sOptics"
  let lookup_class :: String -> TcPluginM Init Class
      lookup_class str =
        tcLookupClass =<<
          lookupOrig sam'sOpticsModule ( mkClsOcc str )
  sam'sGetFieldClass  <- lookup_class "Sam'sGetField"
  sam'sSetFieldClass  <- lookup_class "Sam'sSetField"
  sam'sOpticsClass    <- lookup_class "Sam'sOptics"
  genSam'sOpticsClass <- lookup_class "GenerateSam'sOptics"
  pure ( Sam'sPluginDefs {..} )

sam'sTcPluginSolve :: Sam'sPluginDefs
                   -> [ Ct ] -> [ Ct ]
                   -> TcPluginM Solve TcPluginSolveResult
sam'sTcPluginSolve defs givens wanteds
  -- Simplify Givens.
  | null wanteds
  = return $ TcPluginOk [] []
  -- Solve Wanteds.
  | otherwise
  = do { tcPluginTrace "===== Sam's TcPlugin (start) =====" $
           vcat [ text "Givens:" <+> ppr givens
                , text "Wanteds:" <+> ppr wanteds ]
       ; res <- foldMapM (solveWanted defs givens) wanteds
       ; tcPluginTrace "===== Sam's TcPlugin  (end)  =====" $
           ppr res
       ; return res
       }

--------------------------------------------------------------------------------
-- Typechecking plugin.

solveWanted :: Sam'sPluginDefs
            -> [ Ct ] -> Ct
            -> TcPluginM Solve TcPluginSolveResult
solveWanted defs@( Sam'sPluginDefs {..} ) givens wtd
  = case splitTyConApp_maybe (ctPred wtd) of
      Just (tc, args)
        | tc == classTyCon genSam'sOpticsClass
        , [_rep, arg] <- args
        -> solveSam'sOptics defs givens wtd arg
        | tc == classTyCon sam'sGetFieldClass
        , [_k, _s_rep, _a_rep, fld, s, a] <- args
        -> solveSam'sGetField defs fld s a
        | tc == classTyCon sam'sSetFieldClass
        , [_k, _s_rep, _a_rep, fld, s, a] <- args
        -> solveSam'sSetField defs fld s a
      _ -> return $ TcPluginOk [] []
--- https://github.com/mpickering/lift-plugin/blob/master/lift-plugin/src/LiftPlugin.hs#L268

solveSam'sOptics :: Sam'sPluginDefs
                 -> [ Ct ] -> Ct -> Type
                 -> TcPluginM Solve TcPluginSolveResult
solveSam'sOptics defs@( Sam'sPluginDefs {..} ) _givens wtd arg =
  do { tcPluginTrace "Sam's TcPlugin" $
         vcat [ text "Wanted:" <+> ppr wtd
              , text "orig:" <+> ppr (ctOrigin wtd) ]
     ; case ctOrigin wtd of
         -- Only handle GenerateSam'sOptics Wanted constraints
         -- when they arise as a superclass constraint: this is
         -- the situation we are in when using
         --
         --   > deriving Sam'sOptics via Sam'sPlugin
         { ScOrigin {} ->
  do { case splitTyConApp_maybe arg of
         { Just (tc, tc_args)
           | isDataTyCon tc
           -> addSam'sInstances tc tc_args (tyConDataCons tc)
         ; _ ->
  do { tcPluginTrace "Sam's TcPlugin: not a TyConApp" $
         vcat [ text "Wanted:" <+> ppr wtd ]
     ; let err_msg =   Txt "Cannot derive '" :|: PrintType wtd_predTy :|: Txt "'."
                   :-: Txt "Sam's typechecking plugin only handles data type declarations."
     ; addTypeError wtd err_msg } } }
         ; other_origin ->
  do { tcPluginTrace "Sam's TcPlugin: not a superclass origin" $
         vcat [ text "wtd:" <+> ppr wtd
              , text "orig:" <+> ppr other_origin ]
     ; let ev :: EvTerm
           ev = genSam'sOpticsEvidence defs arg
     ; return $ TcPluginOk [(ev, wtd)] [] } } }
  where
    wtd_predTy :: PredType
    wtd_predTy = ctPred wtd

    addSam'sInstances :: TyCon -> [Type] -> [DataCon]
                      -> TcPluginM Solve TcPluginSolveResult
    addSam'sInstances tc tc_args dcs =
      case dcs of
        -- SLD TODO: I'm only handling data declarations with a single data constructor for now.
        [dc] ->
          case dataConFieldLabels dc of
            -- No field labels: don't do anything.
            []   -> return $ TcPluginOk [] []
            lbls ->
              do { let field_tys = dataConOrigArgTys dc
                       optics_args = SOA { soa_arg = arg, soa_dataCon = dc }
                       insts_args = zipWith3 (sam'sInstanceArguments wtd arg tc tc_args) [0..] field_tys lbls
                       inst_field_tys = map sia_a insts_args
                 ; getField_insts <- mapM (sam'sClassInstance sam'sGetFieldClass (sam'sDataConSelector defs optics_args inst_field_tys)) insts_args
                 ; setField_insts <- mapM (sam'sClassInstance sam'sSetFieldClass (sam'sDataConSetter   defs optics_args inst_field_tys)) insts_args
                 ; unsafeAddGlobalInstances (getField_insts ++ setField_insts)
                 ; tcPluginTrace "Sam's TcPlugin: adding Sam's instances" $
                     vcat [ text "Wanted:" <+> ppr wtd
                          , text "TyCon app:" <+> ppr (tc, tc_args)
                          , text "DataCon:" <+> ppr dc
                          , text "fields:" <+> ppr lbls
                          , text "====="
                          , text "Sam's GetField instances:" <+> ppr getField_insts
                          , text "Sam's SetField instances:" <+> ppr setField_insts ]
                 ; let ev :: EvTerm
                       ev = genSam'sOpticsEvidence defs arg
                 ; return $ TcPluginOk [(ev, wtd)] [] }
        _ -> do { let err_msg =
                           (Txt "Cannot derive '" :|: PrintType (ctPred wtd) :|: Txt "'.")
                       :-: Txt "Sam has only handled data declarations with a single constructor."
                ; addTypeError wtd err_msg }

-- | Solve Sam's GetField constraint using an instance defined by the plugin.
--
-- This is required because we only add instances at the end of typechecking
-- a module, so if we require GetField in the same module as we have derived
-- the instances, then the typechecker won't see them.
solveSam'sGetField :: Sam'sPluginDefs
                   -> Type -- fld
                   -> Type -- s
                   -> Type -- a
                   -> TcPluginM Solve TcPluginSolveResult
solveSam'sGetField _defs (LitTy (StrTyLit fld)) s a
  = do { tcPluginTrace "Sam's TcPlugin: handling GetField" $
           vcat [ text "fld:" <+> ppr fld
                , text "s:" <+> ppr s
                , text "a:" <+> ppr a ]
       ; return $ TcPluginOk [] [] }
         -- TODO: actually do the solving!!
solveSam'sGetField _ _ _ _
  = return $ TcPluginOk [] []

genSam'sOpticsEvidence :: Sam'sPluginDefs -> Type -> EvTerm
genSam'sOpticsEvidence ( Sam'sPluginDefs {..} ) arg =
  let s_rep :: RuntimeRepType
      s_rep = getRuntimeRep arg
  in
    evDataConApp (classDataCon genSam'sOpticsClass)
       [s_rep, arg]
       []

-- | Solve Sam's SetField constraint using an instance defined by the plugin.
--
-- See 'solveSam'sGetField'.
solveSam'sSetField :: Sam'sPluginDefs
                   -> Type -- fld
                   -> Type -- s
                   -> Type -- a
                   -> TcPluginM Solve TcPluginSolveResult
solveSam'sSetField _defs (LitTy (StrTyLit fld)) s a
  = do { tcPluginTrace "Sam's TcPlugin: handling SetField" $
           vcat [ text "fld:" <+> ppr fld
                , text "s:" <+> ppr s
                , text "a:" <+> ppr a ]
       ; return $ TcPluginOk [] [] }
           -- TODO: actually do the solving!!
solveSam'sSetField _ _ _ _
  = return $ TcPluginOk [] []

data Sam'sOpticsArguments
  = SOA
  { soa_arg     :: !Type
  , soa_dataCon :: !DataCon
  }

data Sam'sInstanceArguments
  = SIA
  { sia_fieldLabel :: !FieldLabel
  , sia_fieldIndex :: !Int
  , sia_tyVars     :: ![TcTyVar]
  , sia_inst_tys   :: ![Type]
  , sia_srcSpan    :: !SrcSpan
  , sia_r          :: !Type
  , sia_tc_args    :: ![Type]
  , sia_a_rep      :: !RuntimeRepType
  , sia_a          :: !Type
  }

sam'sInstanceArguments :: Ct -> Type
                       -> TyCon -> [Type]
                       -> Int -> Scaled Type -> FieldLabel
                       -> Sam'sInstanceArguments
sam'sInstanceArguments wtd s tc tc_args i (Scaled _ field_ty) lbl
  = SIA { sia_fieldLabel = lbl
        , sia_fieldIndex = i
        , sia_tyVars     = tyCoVarsOfTypesWellScoped tc_args
        , sia_inst_tys   = [typeSymbolKind, s_rep, a_rep, fld, s, a]
        , sia_srcSpan    = RealSrcSpan (getLclEnvLoc $ ctLocEnv $ ctLoc wtd)
                             Strict.Nothing
        , sia_a_rep      = a_rep
        , sia_a          = a
        , sia_r          = r
        , sia_tc_args    = tc_args }
  where
    tc_tvs :: [TcTyVar]
    tc_tvs = binderVars $ tyConBinders tc
    a :: Type
    a = substTyWith tc_tvs tc_args field_ty
    fld :: Type
    fld = mkStrLitTy $ field_label $ flLabel lbl
    s_rep, a_rep :: RuntimeRepType
    s_rep = getRuntimeRep $ tyConResKind tc
    a_rep = getRuntimeRep a
    r :: Type
    r = mkTyConApp tc tc_args

sam'sClassInstance :: Class
                   -> ( Sam'sInstanceArguments -> TcPluginM Solve CoreExpr )
                   -> Sam'sInstanceArguments
                   -> TcPluginM Solve (ClsInst, CoreExpr)
sam'sClassInstance
  cls
  mk_expr
  sia@( SIA { sia_tyVars     = tvs
            , sia_inst_tys   = inst_tys
            , sia_srcSpan    = src_span } )
  = do { dfun_name <- unsafeLiftTcM $ newDFunName cls inst_tys src_span
       ; inst <- unsafeLiftTcM $ newClsInst Nothing dfun_name tvs [] cls inst_tys
                            -- overlap mode ^^^^^^^               ^^ context (theta)
       ; expr <- mk_expr sia
       ; return (inst, expr) }

sam'sDataConSelector :: Sam'sPluginDefs
                     -> Sam'sOpticsArguments
                     -> [TcType]
                     -> Sam'sInstanceArguments
                     -> TcPluginM Solve CoreExpr
sam'sDataConSelector
  ( Sam'sPluginDefs {..} )
  ( SOA { soa_arg     = arg_ty
        , soa_dataCon = dc } )
  field_tys
  ( SIA { sia_fieldIndex = i
        , sia_tyVars     = tvs
        , sia_inst_tys   = inst_tys
        , sia_a          = a_ty } )
  = do { var <- unsafeLiftTcM $ mkSysLocalM (fsLit "sam'sRec") Many arg_ty
       ; arg_ids <- unsafeLiftTcM $
                      zipWithM
                        (\ fl ty -> mkSysLocalM (field_label $ flLabel fl) Many ty)
                        (dataConFieldLabels dc)
                        field_tys
       ; tcPluginTrace "sam'sDataConSelector" $
          vcat [ text "a_ty:" <+> ppr a_ty
               , text "a_ki:" <+> ppr (typeKind a_ty)
               , text "tvs:" <+> ppr tvs
               , text "field_tys:" <+> ppr field_tys
               , text "arg_ids:" <+> ppr arg_ids
               , text "arg_ids_tys:" <+> ppr (map idType arg_ids) ]
       ; return $
           mkCoreLams tvs $
           mkCoreConApps (classDataCon sam'sGetFieldClass)
               ( map Type inst_tys ++
               [ mkCoreLams [var] $
                 mkWildCase (Var var) (Scaled Many arg_ty) ( field_tys !! i )
                 [ Alt (DataAlt dc) arg_ids
                 $ Var (arg_ids !! i) ] ] )
        }

sam'sDataConSetter :: Sam'sPluginDefs
                   -> Sam'sOpticsArguments
                   -> [TcType]
                   -> Sam'sInstanceArguments
                   -> TcPluginM Solve CoreExpr
sam'sDataConSetter
  ( Sam'sPluginDefs {..} )
  ( SOA { soa_arg     = arg_ty
        , soa_dataCon = dc } )
  field_tys
  ( SIA { sia_fieldIndex = i
        , sia_tyVars     = tvs
        , sia_inst_tys   = inst_tys
        , sia_r          = r_ty
        , sia_a          = a_ty
        , sia_tc_args    = tc_args } )
  = do { rec_var <- unsafeLiftTcM $ mkSysLocalM (fsLit "sam'sRec") Many r_ty
       ; arg_var <- unsafeLiftTcM $ mkSysLocalM (fsLit "sam'sArg") Many a_ty
       ; arg_ids <- unsafeLiftTcM $
                      zipWithM
                        (\ fl ty -> mkSysLocalM (field_label $ flLabel fl) Many ty)
                        (dataConFieldLabels dc)
                        field_tys
       ; tcPluginTrace "sam'sDataConSetter" $
          vcat [ text "a_ty:" <+> ppr a_ty
               , text "a_ki:" <+> ppr (typeKind a_ty)
               , text "tvs:" <+> ppr tvs
               , text "field_tys:" <+> ppr field_tys
               , text "arg_ids:" <+> ppr arg_ids
               , text "arg_ids_tys:" <+> ppr (map idType arg_ids)
               , text "r_ty:" <+> ppr r_ty
               , text "inst_tys:" <+> ppr inst_tys ]
       ; return $
           mkCoreLams tvs $
           mkCoreConApps (classDataCon sam'sSetFieldClass)
               ( map Type inst_tys ++
               [ mkCoreLams [arg_var, rec_var] $
                 mkWildCase (Var rec_var) (Scaled Many arg_ty) r_ty
                 [ Alt (DataAlt dc) arg_ids
                 $ mkCoreConApps dc ( map Type tc_args
                                    ++ setAt i (Var arg_var) ( map Var arg_ids ) )
                 ] ] )
        }

{-# NOINLINE sam'sIORef #-}
sam'sIORef :: IORef (ModuleEnv [(ClsInst, CoreExpr)])
sam'sIORef = unsafePerformIO $ newIORef emptyModuleEnv

unsafeAddGlobalInstances :: [(ClsInst, CoreExpr)] -> TcPluginM Solve ()
unsafeAddGlobalInstances new_insts = unsafeLiftTcM $
  do { curr_mod <- getModule
     ; liftIO $
  do { atomicModifyIORef sam'sIORef $ \ mod_bnds ->
         let !mod_bnds' = extendModuleEnvWith (++) mod_bnds curr_mod new_insts
         in (mod_bnds', ()) } }

-- | Add a type error with the given error message.
addTypeError :: Ct -> TcPluginErrorMessage -> TcPluginM Solve TcPluginSolveResult
addTypeError wtd err_msg
  = do { err_pty <- mkTcPluginErrorTy err_msg
       ; let
           err_loc :: CtLoc
           err_loc = bumpCtLocDepth $ ctLoc wtd
       ; err_ctEv <- setCtLocM err_loc $ newWanted err_loc err_pty
       ; let err_ct = mkNonCanonical err_ctEv
       ; return $ TcPluginContradiction [err_ct] }

---------------------------------------
-- Typecheck result action plugin

sam'sTcGblEnvPlugin :: TcGblEnv -> TcM TcGblEnv
sam'sTcGblEnvPlugin env@( TcGblEnv { tcg_insts = old_insts, tcg_binds = old_binds })
  = do { curr_mod <- getModule
       ; uniqs <- getUniquesM
       ; sam's_insts <-  ( ( `orElse` [] ) . ( `lookupModuleEnv` curr_mod ) )
                     <$> liftIO (readIORef sam'sIORef)
       ; let sam's_binds = zipWith mk_bind uniqs sam's_insts
       ; traceTc "Sam's plugin: adding instances" $
           vcat ( map ppr $ zip sam's_insts sam's_binds )
       ; return $ env { tcg_insts = old_insts ++ map fst sam's_insts
                      , tcg_binds = old_binds `unionBags` listToBag sam's_binds } }
  where
    mk_bind :: Unique -> (ClsInst, CoreExpr) -> LHsBind GhcTc
    mk_bind uniq (inst, expr) =
      noLocA $ VarBind noExtField bind_id
        ( noLocA $ mkHsWrap ( WpLet $ EvBinds $ unitBag
                       $ EvBind bind_id' (EvExpr expr) True )
                       ( HsVar noExtField $ noLocA bind_id' )
        )
      where bind_id  = is_dfun inst
            bind_id' = localiseId $ setVarUnique bind_id uniq

---------------------------------------
-- Util.

instance Semigroup TcPluginSolveResult where
  TcPluginSolveResult a1 b1 c1 <> TcPluginSolveResult a2 b2 c2
    = TcPluginSolveResult (a1 <> a2) (b1 <> b2) (c1 <> c2)
instance Monoid TcPluginSolveResult where
  mempty = TcPluginSolveResult mempty mempty mempty
instance Outputable TcPluginSolveResult where
  ppr (TcPluginSolveResult a b c) =
    text "TcPluginSolveResult" <+>
      ( braces $ vcat
         [ text "insols:" <+> ppr a
         , text "solved:" <+> ppr b
         , text "new:"    <+> ppr c ]
      )

foldMapM :: forall f a m b
         . (Foldable f, Monoid b, Monad m) => (a -> m b) -> f a -> m b
foldMapM f as = coerce $ foldMap (Ap . f) as

setAt :: forall a. Int -> a -> [a] -> [a]
setAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go :: Int -> [a] -> [a]
    go 0 (_:xs) = a : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []
