module Tiny.CHI.Eval
  ( EvalState
  , RuntimeError
  , initialEvalState
  , eval
  , evalExp
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable
import Data.IntMap ( IntMap )
import Data.List
import Data.Map ( (!) )
import Data.Maybe
import Tiny.CHI.Anno
import Tiny.CHI.Builtins
import Tiny.CHI.Types
import Tiny.CHI.Util
import Tiny.CHI.Value
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

newtype RuntimeError = RuntimeError { _msg :: String }

instance Show RuntimeError where
  show (RuntimeError msg) = msg

data EvalState
  = EvalState
  { eval_snapshots :: IntMap [Env]
  -- ^ snapshots of the environment at certain expressions
  , eval_profile_samples :: IntMap Int
  -- ^ how many times was the most popular expression evaluated?
  , eval_max_n_samples :: Int
  , eval_test_case_results :: IntMap Bool
  }

initialEvalState :: EvalState
initialEvalState =
  EvalState
  { eval_snapshots = IntMap.empty
  , eval_profile_samples = IntMap.empty
  , eval_max_n_samples = 0
  , eval_test_case_results = IntMap.empty
  }

type EvalNoExcept = StateT EvalState IO

type Eval = ExceptT RuntimeError EvalNoExcept

valueZonkTransform
  :: TransformOps TypecheckPhase EvalPhase EvalNoExcept
valueZonkTransform =
  TransformOps
  { t_ident = pure
  , t_op = doNothing
  , t_exp = zonkTransformExp
  , t_xexp = doNothing
  , t_pat = doNothing
  , t_xpat = doNothing
  , t_decl = pure
  , t_xdecl = doNothing
  , t_typ = doNothing
  , t_xtyp = doNothing
  , t_knd = doNothing
  , t_xknd = doNothing
  , t_do = zonkTransformDoStmt
  , t_xdo = doNothing
  , t_topDecl = doNothing
  , t_xtopDecl = doNothing
  , t_sourceFile = doNothing
  }
  where
    doNothing = const (pure ())

zonkTransformExp
  :: XExp TypecheckPhase -> EvalNoExcept (XExp EvalPhase)
zonkTransformExp (TcExpInfo { tc_traceId }) = do
  sampleMap <- gets eval_profile_samples
  let TraceId tid = tc_traceId
  maybeSnapshotEnvs <- gets (IntMap.lookup tid . eval_snapshots)
  maybeTestPassed <- gets (IntMap.lookup tid . eval_test_case_results)
  totalNSamples <- gets (fromIntegral . eval_max_n_samples)
  case IntMap.lookup tid sampleMap of
    Just nSamples -> pure EvalExpInfo
      { ev_exp_traceId = tc_traceId
      , ev_exp_sample_percentage = fromIntegral nSamples / totalNSamples
      , ev_exp_snapshot = envsToSnapshot <$> maybeSnapshotEnvs
      , ev_exp_test_status = testStatus maybeTestPassed
      }
    Nothing -> pure EvalExpInfo
      { ev_exp_traceId = tc_traceId
      , ev_exp_sample_percentage = 0.0
      , ev_exp_snapshot = envsToSnapshot <$> maybeSnapshotEnvs
      , ev_exp_test_status = testStatus maybeTestPassed
      }

testStatus :: Maybe Bool -> TestCaseStatus
testStatus Nothing = NotRun
testStatus (Just True) = Passed
testStatus (Just False) = Failed

zonkTransformDoStmt
  :: XDo TypecheckPhase -> EvalNoExcept (XDo EvalPhase)
zonkTransformDoStmt traceId@(TraceId tid) = do
  maybeSnapshotEnvs <- gets (IntMap.lookup tid . eval_snapshots)
  pure EvalDoStmtInfo
    { ev_do_traceId = traceId
    , ev_do_snapshot = envsToSnapshot <$> maybeSnapshotEnvs
    }

envsToSnapshot :: [Env] -> Snapshot
envsToSnapshot envs =
  let
    variables = Map.keys (head envs)
    table = map (map show . Map.elems) envs
  in Snapshot variables table

-- | Take a typechecked source file and annotate it with some
--   additional information recording the runtime execution of the
--   source file using the state accumulated in the Eval monad.
valueZonk :: SourceFile TypecheckPhase -> EvalNoExcept (SourceFile EvalPhase)
valueZonk = transformAnno valueZonkTransform

snapshotEnv :: TraceId -> Env -> Eval ()
snapshotEnv (TraceId tid) env =
  modify (\s -> s { eval_snapshots =
                    IntMap.alter alterer tid (eval_snapshots s) })
  where
    alterer = Just <$> maybe [env] (++[env])

snapshotEnvAtExp :: XExp TypecheckPhase -> Env -> Eval ()
snapshotEnvAtExp (TcExpInfo { tc_traceId }) = snapshotEnv tc_traceId

setTestCaseStatus :: TraceId -> Bool -> Eval ()
setTestCaseStatus (TraceId tid) result =
  modify (\s -> s { eval_test_case_results =
                    IntMap.insert tid result (eval_test_case_results s) })

setTestCaseStatusAtExp :: XExp TypecheckPhase -> Bool -> Eval ()
setTestCaseStatusAtExp (TcExpInfo { tc_traceId }) = setTestCaseStatus tc_traceId

snapshotEnvAtDoStmt :: XDo TypecheckPhase -> Env -> Eval ()
snapshotEnvAtDoStmt = snapshotEnv

sample :: XExp TypecheckPhase -> Eval ()
sample (TcExpInfo { tc_traceId }) = do
  map <- gets eval_profile_samples
  let newValue = maybe 1 (+1) (IntMap.lookup tid map)
  modify (\s -> s { eval_profile_samples = IntMap.insert tid newValue map
                  , eval_max_n_samples = max newValue (eval_max_n_samples s) })
  where
    TraceId tid = tc_traceId

eval
  :: SourceFile TypecheckPhase
  -> IO ( Maybe RuntimeError, SourceFile EvalPhase )
eval s@(SourceFile _ topDecls _) =
  do
    (env, eval'd) <- evalStateT action initialEvalState
    case env of
      Left err -> pure (Just err, eval'd)
      Right env -> do
        case Map.lookup "main" env of
          Just (VIO a) -> a >> pure (Nothing, eval'd)
          Just _ -> pure (Just (RuntimeError "main is not IO"), eval'd)
          Nothing -> pure (Just (RuntimeError "no main!"), eval'd)
  where
    action = do
      envOrErr <- runExceptT $ evalDecls Map.empty decls
      eval'd <- valueZonk s
      pure (envOrErr, eval'd)

    decls = mapMaybe filterTopDeclDecls topDecls

    filterTopDeclDecls :: TopDecl TypecheckPhase -> Maybe (Decl TypecheckPhase)
    filterTopDeclDecls (Located _ (Decl _ d)) = Just d
    filterTopDeclDecls _ = Nothing

-- | Assuming `e' is a EVar, find it's name
asVar :: Exp TypecheckPhase -> Maybe String
asVar (Located _ (EVar _ (Located _ (Ident _ id)))) = Just id
asVar _ = Nothing

-- | lookup a builtin, and return the function that implements
--   it along with the function's name.
lookupBuiltin :: String -> Maybe ([Value] -> Value, String)
lookupBuiltin name =
  fmap ((,name) . fst) (Map.lookup name builtins)

-- | evalExp maps a type-checked expression to a value, in the monad
--   Eval. An additional pass turns a type-checked expression with the
--   mapping generated by evalExp into a eval'ed expression.
evalExp :: Env -> Exp TypecheckPhase -> Eval Value
evalExp env (Located _ e) = go e
  where
    go :: Exp_ TypecheckPhase -> Eval Value
    go (EInt tcInfo i) = sample tcInfo >> pure (VInt i)
    go (EChar tcInfo (_, c, _)) = sample tcInfo >> pure (VChar c)
    go (EString tcInfo (_, s, _)) = sample tcInfo >> pure (VList (map VChar s))
    go (ENeg tcInfo (_, e)) = do
      sample tcInfo
      eV <- evalExp env e
      case eV of
        VInt i -> pure $ VInt (-i)
        _ -> error "bug in tc"
    go (EVar tcInfo (Located _ (Ident _ id))) = do
      sample tcInfo
      case Map.lookup id env of
        Just value -> pure value
        Nothing -> throwError (RuntimeError ("unbound " ++ id))
    go (EFun tcInfo (_, ps, _, body)) =
      sample tcInfo >> pure (VClo $ Clo ps env body)
    go (EApp tcInfo (e, es)) = do
      sample tcInfo
      case lookupBuiltin =<< asVar e of
        Just (builtin, nm) -> do
          ves <- mapM (evalExp env) es
          let res = builtin ves
          when (nm == "trace") $ do
            snapshotEnvAtExp tcInfo (Map.fromList [("value", res)])
          when (nm == "assertEq") $ do
            case res of
              VBool True ->
                setTestCaseStatusAtExp tcInfo True
              _ -> do
                snapshotEnv (let Located _ e = head es in idOfExp e)
                  (Map.fromList [("value", head ves)])
                setTestCaseStatusAtExp tcInfo False
          pure res
        Nothing -> do
          ve <- evalExp env e
          ves <- mapM (evalExp env) es
          (fEnv, body) <- case ve of
            (VClo (Clo ps env' body)) -> do
              -- FIXME: Zipping here is an issue if there are fewer
              --        arguments ('ves') than there are patterns.
              --        Then, pattern assignments are going to get
              --        dropped.  We need to potentially return a new
              --        closure from this function as opposed to
              --        always evaluating the body (we should only
              --        eval the body if there are no more closure
              --        parameters to bind).
              let results = allOrNothing $ zipWith evalPattern ps ves
                  maybeFormals = fmap Map.unions results
              case maybeFormals of
                Just formals -> pure (Map.union formals env', body)
                Nothing -> throwError (RuntimeError "pattern match failed at function application")
            (VCloFix nm closures) -> do
              let (Clo ps env' body) = closures ! nm
                  results = allOrNothing $ zipWith evalPattern ps ves
                  maybeFormals = fmap Map.unions results
              case maybeFormals of
                Just formals ->
                  pure (Map.unions
                        [ formals
                        , env'
                        , Map.mapWithKey (\k _ -> VCloFix k closures) closures ]
                       , body)
                Nothing -> throwError (RuntimeError "pattern match failed at function application")
            _ -> throwError (RuntimeError "cannot apply non-function")
          evalExp fEnv body
    go (EBinop tcInfo (lhs, Located _ (Op _ op), rhs)) = do
      sample tcInfo
      lhsv <- evalExp env lhs
      rhsv <- evalExp env rhs
      case Map.lookup op builtins of
        Just (f, _) -> pure $ f [lhsv, rhsv]
        Nothing -> do
          ves <- mapM (evalExp env) [lhs, rhs]
          (fEnv, body) <- case Map.lookup op env of
            (Just (VClo (Clo ps env' body))) -> do
              let results = allOrNothing $ zipWith evalPattern ps ves
                  maybeFormals = fmap Map.unions results
              case maybeFormals of
                Just formals -> pure (Map.union formals env', body)
                Nothing -> throwError (RuntimeError "pattern match failed at function application")
            (Just (VCloFix nm closures)) -> do
              let (Clo ps env' body) = closures ! nm
                  results = allOrNothing $ zipWith evalPattern ps ves
                  maybeFormals = fmap Map.unions results
              case maybeFormals of
                Just formals ->
                  pure (Map.unions
                        [ formals
                        , env'
                        , Map.mapWithKey (\k _ -> VCloFix k closures) closures ]
                       , body)
                Nothing -> throwError (RuntimeError "pattern match failed at function application")
            Nothing -> throwError (RuntimeError ("unbound operator" ++ op))
            _ -> throwError (RuntimeError "cannot apply non-function")
          evalExp fEnv body
    go (ELet tcInfo (_, decls, _, body)) = do
      sample tcInfo
      -- TODO: Dependency analysis. This assumes top-down deps
      env' <- evalDecls env decls
      evalExp env' body
    go (ETuple tcInfo (_, es, _)) = do
      sample tcInfo
      vs <- mapM (evalExp env) es
      pure $ VTup (foldr (:) [] vs)
    go (EList tcInfo (_, es, _)) = do
      sample tcInfo
      vs <- mapM (evalExp env) es
      pure $ VList (foldr (:) [] vs)
    go (EParens tcInfo (_, e, _)) = sample tcInfo >> evalExp env e
    go (EIf tcInfo (_, c, _, thenE, _ , elseE)) = do
      sample tcInfo
      cv <- evalExp env c
      case cv of
        VBool True -> evalExp env thenE
        VBool False -> evalExp env elseE
        _ -> error "bug in tc (eval If)"
    go (ECase tcInfo (_, e, _, cases)) = do
      sample tcInfo
      ev <- evalExp env e
      goCase ev cases
      where
        goCase _ [] = throwError (RuntimeError "no case matched")
        goCase v ((p, _, body):cs) =
          case evalPattern p v of
            Just bound -> evalExp (Map.union bound env) body
            Nothing -> goCase v cs
    go (EDo _ (_, stmts)) = do
      let (ds, e) = (init stmts, last stmts)
      env' <- foldlM evalDoStmt env ds
      case e of
        Located _ (DoExp tid e) -> do
          snapshotEnvAtDoStmt tid env'
          evalExp env' e
        _ -> error "bug in tc (EDo)"
    go (ETypArg _ _) = undefined
    go (EAnnot _ (e, _, _)) = evalExp env e
    go (XExp _) = undefined

evalDoStmt
  :: Env
  -> DoStmt TypecheckPhase
  -> Eval Env
evalDoStmt env (Located loc stmt) = go stmt
  where
    go :: DoStmt_ TypecheckPhase -> Eval Env
    go (DoExp tid e) = do
      VIO vEIO <- evalExp env e
      void $ liftIO vEIO
      env <$ snapshotEnvAtDoStmt tid env
    go (DoBind tid (p, _, e)) = do
      VIO vEIO <- evalExp env e
      vE <- liftIO vEIO
      case evalPattern p vE of
        Just env' ->
          let env'' = env `Map.union` env'
          in env'' <$ snapshotEnvAtDoStmt tid env''
        Nothing -> throwError (RuntimeError "do bind pattern match failure")
    go (DoLet tid (_, ds)) = do
      env' <- evalDecls env ds
      env' <$ snapshotEnvAtDoStmt tid env'
    go (XDo _) = undefined

isFunctionDecl :: Decl TypecheckPhase -> Bool
isFunctionDecl (Located _ (Equation _ (lhs, _, _))) =
  case lhs of
    (Right _) -> True
    (Left (Located _ (PVar _ (Located _ (Ident _ _))), _:_)) -> True
    _ -> False
isFunctionDecl _ = False

-- | extend `env` with the bindings bound in the declarations.
--   TODO: This function does not perform dependency analysis! Further,
--         it requires that the definitions of mutually recursive functions
--         be next to each other!
evalDecls :: Env -> [Decl TypecheckPhase] -> Eval Env
evalDecls env decls = foldlM handleGroup env groups
  where
    handleGroup :: Env -> [Decl TypecheckPhase] -> Eval Env
    handleGroup env [] = pure env
    handleGroup env ds@(d:_)
      | isFunctionDecl d = handleFunctionDeclGroup env ds
      | otherwise = handleBindingGroup env ds

    -- | handleFunctionDeclGroup processes a list
    --   of function declarations as if they are mutually recursive.
    handleFunctionDeclGroup :: Env -> [Decl TypecheckPhase] -> Eval Env
    handleFunctionDeclGroup env ds = do
      closures <- mapM closureOfDecl ds
      let closuresMap = Map.fromList closures
      pure $ foldl (\env' (nm, _) ->
                      Map.insert nm (VCloFix nm closuresMap) env') env closures
        where
          closureOfDecl (Located _ (Equation _ (lhs, (_, rhse), maybeWhere))) = do
            env' <- case maybeWhere of
              Just (_, ds) -> evalDecls env ds
              Nothing -> pure env
            let (nm, ps) = nameAndPatternsOfLhs lhs
            pure (nm, Clo ps env' rhse)
          closureOfDecl _ =
            error "non-function decl encountered in handleFunctionDeclGroup"

    -- | handleBindingGroup processes a list of
    --   non-function declarations in the order of definition.
    handleBindingGroup :: Env -> [Decl TypecheckPhase] -> Eval Env
    handleBindingGroup env [] = pure env
    handleBindingGroup env (d:ds) =
      case d of
        (Located _ (Equation _ (lhs, (_, rhse), maybeWhere))) -> do
          env' <- case maybeWhere of
            Just (_, ds) -> evalDecls env ds
            Nothing -> pure env

          case lhs of
            (Left (p, [])) -> do
              rhsv <- evalExp env' rhse
              case evalPattern p rhsv of
                Just env'' -> handleBindingGroup (Map.union env env'') ds
                Nothing -> throwError (RuntimeError "pattern match failure")
            _ -> error "non-binding encountered in handleBindingGroup"
        (Located _ (Signature _ _)) ->
          handleBindingGroup env ds
        (Located _ (XDecl _)) ->
          handleBindingGroup env ds

    groups = groupBy (\a b -> isFunctionDecl a && isFunctionDecl b) decls

-- | return a new `Env` representing the values bound by the
--   pattern, or `Nothing` if the pattern match failed.
evalPattern :: Pat TypecheckPhase -> Value -> Maybe Env
evalPattern (Located _ p) = go p
  where
    go :: Pat_ TypecheckPhase -> Value -> Maybe Env
    go (PInt _ i) (VInt j)
      | i == j = Just Map.empty
      | otherwise = Nothing
    go (PVar _ (Located _ (Ident _ nm))) v =
      Just $ Map.singleton nm v
    go (PApp _ _) _ = undefined
    go (PTuple _ (_, ps, _)) (VTup vs) = do
      let asList = foldr (:) [] ps
          maybeEnvs = allOrNothing $ zipWith evalPattern asList vs
      guard (length asList == length vs)
      Map.unions <$> maybeEnvs
    go (PList _ (_, ps, _)) (VList vs) = do
      let asList = foldr (:) [] ps
          maybeEnvs = allOrNothing $ zipWith evalPattern asList vs
      guard (length asList == length vs)
      Map.unions <$> maybeEnvs
    go (PCons _ (lhs, _, rhs)) (VList (v:vs)) =
      Map.union <$> evalPattern lhs v <*> evalPattern rhs (VList vs)
    go (PParens _ (_, p, _)) v =
      evalPattern p v
    go (PWild _ _) _ = Just Map.empty
    go _ _ = Nothing

allOrNothing :: [Maybe a] -> Maybe [a]
allOrNothing [] = Just []
allOrNothing (Nothing:_) = Nothing
allOrNothing (Just a:as) =
  case allOrNothing as of
    Nothing -> Nothing
    Just as -> Just (a:as)
