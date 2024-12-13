module Tiny.CHI.Typecheck
  ( typecheck
  , typecheckExp
  , TcTyp(..)
  , TcForall(..)
  , TypecheckError(..)
  , pprintScheme
  , pprintType
  , defaultNameMap ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable
import Data.IntMap ( IntMap )
import Data.IntSet ( IntSet )
import Data.List
import Data.Map ( Map )
import Data.Maybe
import Tiny.CHI.Anno
import Tiny.CHI.Builtins ( builtinsGamma )
import Tiny.CHI.Typecheck.Types
import Tiny.CHI.Types
import Tiny.CHI.Util
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

defaultNameMap :: [(Int, String)]
defaultNameMap = zip [0..] nameStream

pprintType :: TcTyp -> [(Int, String)] -> String
pprintType (TcCon nm) _ = nm
pprintType (TcApp (TcCon "Function") [a, b]) nameMap -- special case for functions
  | isFunType a = "(" ++ pprintType a nameMap ++ ") -> " ++ pprintType b nameMap
  | otherwise = pprintType a nameMap ++ " -> " ++ pprintType b nameMap
pprintType (TcApp (TcCon "Tuple") ts) nameMap = -- special case for tuples
  "(" ++ intercalate ", " (map (`pprintType` nameMap) ts) ++ ")"
pprintType (TcApp (TcCon "List") [t]) nameMap = -- special case for lists
  "[" ++ pprintType t nameMap ++ "]"
pprintType (TcApp t ts) nameMap =
  "(" ++ unwords (map (`pprintType` nameMap) (t:ts)) ++ ")"
pprintType (TcVar v) nameMap = fromJust $ lookup v nameMap

pprintScheme :: TcForall -> String
pprintScheme (TcForall bound t)
  | IntSet.null bound = pprintType t defaultNameMap
  | otherwise = "forall " ++ boundVarsString ++ " . " ++ pprintType t nameMap
  where
    minVar = IntSet.findMin bound
    varsList = IntSet.toList bound
    centeredVars = map (subtract minVar) varsList
    nameMap = zip varsList (map (nameStream !!) centeredVars)
    boundVarsString = unwords $ map (nameStream !!) centeredVars

data TypecheckError
  = UnificationError TraceId TcTyp TcTyp
  | UnboundVariableError TraceId String Range

instance Show TypecheckError where
  show (UnificationError _ a b) =
    "could not unify "
    ++ pprintType a defaultNameMap
    ++ " and "
    ++ pprintType b defaultNameMap
  show (UnboundVariableError _ id range) =
    "unbound variable " ++ id ++ " at " ++ show range

data Constraint = Eq TcTyp TcTyp TraceId
  deriving Show

type Subst = IntMap TcTyp

domainSubst :: Subst -> IntSet
domainSubst = IntMap.keysSet

data TcState
  = TcState
  { tc_constraints :: [Constraint]
  , tc_fresh_tyvar :: Int
  , tc_unsatisfiable_constraints :: [Constraint]
  , tc_global_subst :: Subst
  , tc_trace_id_to_type :: IntMap TcTyp
  , tc_errors :: [TypecheckError]
  }

assignTypeOfTerm :: TraceId -> TcTyp -> Typecheck ()
assignTypeOfTerm (TraceId id) typ =
  modify (\s -> s { tc_trace_id_to_type =
                    IntMap.insert id typ (tc_trace_id_to_type s) })

type Typecheck = State TcState

freshTyVar :: Typecheck TcTyp
freshTyVar = do
  idx <- gets tc_fresh_tyvar
  modify $ \s -> s { tc_fresh_tyvar = tc_fresh_tyvar s + 1 }
  pure $ TcVar idx

typecheck
  :: SourceFile RenamePhase
  -> ( [TypecheckError]
     , SourceFile TypecheckPhase )
typecheck s@(SourceFile _ topDecls _) = runTypechecker action
  where
    action = do
      void $ solveSubproblem (tcDecls (TraceId (-1)) builtinsGamma decls) -- FIXME: Use a valid TraceId
      zonk s

    decls = mapMaybe filterTopDeclDecls topDecls

    filterTopDeclDecls :: TopDecl RenamePhase -> Maybe (Decl RenamePhase)
    filterTopDeclDecls (Located _ (Decl _ d)) = Just d
    filterTopDeclDecls _ = Nothing

zonkTransform :: TransformOps RenamePhase TypecheckPhase Typecheck
zonkTransform =
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
  , t_do = pure
  , t_xdo = doNothing
  , t_topDecl = doNothing
  , t_xtopDecl = doNothing
  , t_sourceFile = doNothing
  }
  where
    doNothing = const (pure ())

zonkTransformExp :: XExp RenamePhase -> Typecheck (XExp TypecheckPhase)
zonkTransformExp tid@(TraceId id) = do
  map <- gets tc_trace_id_to_type
  subst <- gets tc_global_subst
  case IntMap.lookup id map of
    Just typ ->
      let substTyp = applySubst typ subst
      in pure TcExpInfo
         { tc_traceId = tid
         , tc_typ_string = pprintType substTyp defaultNameMap
         , tc_typ = substTyp
         }
    Nothing ->
      pure TcExpInfo
      { tc_traceId = tid
      , tc_typ_string = pprintType (TcVar 0) defaultNameMap
      , tc_typ = TcVar 0
      }

-- | Take a typechecked source file, and annotate each expression
--   with its type using the state accumulated in the Typecheck
--   monad during typechecking.
zonk :: SourceFile RenamePhase -> Typecheck (SourceFile TypecheckPhase)
zonk = transformAnno zonkTransform

-- | See `zonk`
zonkExp :: Exp RenamePhase -> Typecheck (Exp TypecheckPhase)
zonkExp = transformExp zonkTransform

type Gamma = Map String TcForall

eq :: TcTyp -> TcTyp -> TraceId -> Typecheck ()
eq a b tid = modify (\s -> s { tc_constraints = Eq a b tid : tc_constraints s })

addConstraints :: [Constraint] -> Typecheck ()
addConstraints cs = modify (\s -> s { tc_constraints = cs ++ tc_constraints s })

-- | the variables of a type
varsT :: TcTyp -> IntSet
varsT (TcCon _) = IntSet.empty
varsT (TcApp _ subterms) = IntSet.unions (map varsT subterms)
varsT (TcVar i) = IntSet.singleton i

-- | the variables of a constraint
varsC :: Constraint -> IntSet
varsC (Eq t1 t2 _) = IntSet.union (varsT t1) (varsT t2)

-- | the variables of a constraint set
varsG :: [Constraint] -> IntSet
varsG cs = IntSet.unions (map varsC cs)

-- | substitute `forT` in for `i` in the term.
substT :: Int -> TcTyp -> TcTyp -> TcTyp
substT i forT (TcApp t ts) = TcApp (substT i forT t) (map (substT i forT) ts)
substT _    _ (TcCon nm) = TcCon nm
substT i forT (TcVar j)
  | i == j = forT
  | otherwise = TcVar j

-- | substitute `forT` in for `i` in the constraint
substC :: Int -> TcTyp -> Constraint -> Constraint
substC i forT (Eq t1 t2 tid) = Eq (substT i forT t1) (substT i forT t2) tid

-- | substitute `forT` in for `i` in the constraint set
substG :: Int -> TcTyp -> [Constraint] -> [Constraint]
substG i forT = map (substC i forT)

freeTyVarsScheme :: TcForall -> IntSet
freeTyVarsScheme (TcForall bound t) = IntSet.difference (varsT t) bound

freeTyVarsGamma :: Gamma -> IntSet
freeTyVarsGamma gamma = IntSet.unions (map freeTyVarsScheme (Map.elems gamma))

freeTyVarsConstraintSet :: [Constraint] -> IntSet
freeTyVarsConstraintSet [] = IntSet.empty
freeTyVarsConstraintSet ((Eq t1 t2 _) : rest) =
  IntSet.unions [varsT t1, varsT t2, freeTyVarsConstraintSet rest]

addUnsatisfiable :: Constraint -> Typecheck ()
addUnsatisfiable c =
  modify (\s -> s { tc_unsatisfiable_constraints =
                    c:tc_unsatisfiable_constraints s })

addError :: TypecheckError -> Typecheck ()
addError e = modify (\s -> s { tc_errors = e:tc_errors s })

unifyStep :: [Constraint] -> Typecheck (Maybe [Constraint])
unifyStep (c@(Eq (TcApp t ts) (TcApp t' ts') tid) : rest)
  | t == t' && ts == ts' = pure $ Just rest
  | length (t:ts) == length (t':ts') =
      pure $ Just $ zipWith (\a b -> Eq a b tid) (t:ts) (t':ts') ++ rest
  | otherwise = addUnsatisfiable c >> pure (Just rest)
unifyStep (c@(Eq (TcCon nm) (TcCon nm') _) : rest)
  | nm == nm' = pure $ Just rest
  | otherwise = addUnsatisfiable c >> pure (Just rest)
unifyStep ((Eq t@(TcCon _) v@(TcVar _) tid) : rest) =
  pure $ Just $ Eq v t tid : rest
unifyStep ((Eq t@(TcApp _ _) v@(TcVar _) tid) : rest) =
  pure $ Just $ Eq v t tid : rest
unifyStep (c@(Eq (TcVar i) t@(TcVar j) _) : rest)
  | i == j = pure $ Just rest
  | i /= j && i `IntSet.member` varsG rest =
      pure $ Just $ c : substG i t rest
  | otherwise = pure Nothing
unifyStep (c@(Eq (TcVar i) t _) : rest)
  | i `IntSet.notMember` varsT t && i `IntSet.member` varsG rest =
      pure $ Just $ c : substG i t rest
  | i `IntSet.member` varsT t = addUnsatisfiable c >> pure (Just rest)
  | otherwise = pure Nothing
unifyStep (c@(Eq (TcCon _) (TcApp _ _) _) : rest) =
  addUnsatisfiable c >> pure (Just rest)
unifyStep (c@(Eq (TcApp _ _) (TcCon _) _) : rest) =
  addUnsatisfiable c >> pure (Just rest)
unifyStep [] = pure (Just [])

unify :: [Constraint] -> Typecheck [Constraint]
unify cs = do
  -- apply until fixed-point
  res <- tryApplyRule [] cs
  case res of
    Just cs' -> unify cs'
    Nothing -> pure cs
  where
    -- | Try to apply a reduction by calling unifyStep
    --   on each constraint in turn. Return `Just cs`
    --   where cs is the new set of constraints if the step
    --   made a reduction. If no constraints could be reduced,
    --   return Nothing.
    tryApplyRule left (focus : right) = do
      maybeCs <- unifyStep (focus : (left ++ right))
      case maybeCs of
        Just cs' -> pure $ Just cs'
        Nothing -> tryApplyRule (focus : left) right
    tryApplyRule _ [] = pure Nothing

-- | A set of constraints which is solved by unification
--   should have the form [v1 :~~: t1, v2 :~~: t2, ... ].
--   This function turns a constraint set of this form into
--   a proper substitution.
asSubstitution :: [Constraint] -> Subst
asSubstitution (Eq (TcVar i) t _ : rest) =
  IntMap.insert i t $ asSubstitution rest
asSubstitution [] = IntMap.empty
asSubstitution v = error ("bug in unification (" ++ show v ++ ")")

applySubstC :: Constraint -> Subst -> Constraint
applySubstC (Eq a b tid) subst =
  Eq (applySubst a subst) (applySubst b subst) tid

applySubst :: TcTyp -> Subst -> TcTyp
applySubst (TcCon nm) _ = TcCon nm
applySubst (TcApp t ts) subst =
  TcApp (applySubst t subst) (map (`applySubst` subst) ts)
applySubst (TcVar i) subst =
  case IntMap.lookup i subst of
    Just t' -> t'
    Nothing -> TcVar i

applySubst1 :: Int -> Subst -> TcTyp
applySubst1 alpha s =
  case IntMap.lookup alpha s of
    Just t -> t
    Nothing -> TcVar alpha

-- | Solve the constraints in the Typecheck action yielding a
--   substitution. There are no existing constraints in the
--   subproblem, and constraints are not propagated out of the
--   subproblem. But, variables free in the subproblem are also
--   guaranteed to be free in the caller's problem.
solveSubproblem :: Typecheck a -> Typecheck (a, Subst)
solveSubproblem tca = do
  state <- get
  (a, subproblemState) <- lift $ runStateT tca (state { tc_constraints = [] })
  put $ subproblemState { tc_constraints = tc_constraints state }
  subst <- asSubstitution <$> unify (tc_constraints subproblemState)
  modify (\s -> s { tc_global_subst = tc_global_subst s `IntMap.union` subst })
  pure (a, subst)

runTypechecker :: Typecheck a -> ([TypecheckError], a)
runTypechecker tca = (allErrors, res)
  where
    allErrors = tcErrors ++ tc_errors tcState

    tcErrors = map (\(Eq a b tid) -> UnificationError tid a b) instantiated

    globalSubst = tc_global_subst tcState
    unsatisfiable = tc_unsatisfiable_constraints tcState
    instantiated = map (`applySubstC` globalSubst) unsatisfiable

    (res, tcState) = runState tca initState

    initState = TcState
      { tc_constraints = []
      , tc_fresh_tyvar = 0
      , tc_unsatisfiable_constraints = []
      , tc_global_subst = IntMap.empty
      , tc_trace_id_to_type = IntMap.empty
      , tc_errors = []
      }

generalize :: TcTyp -> IntSet -> TcForall
generalize t bound = TcForall alphas t
  where
    alphas = IntSet.difference (varsT t) bound

typecheckExp
  :: Exp RenamePhase
  -> ( [TypecheckError]
     , TcForall              -- ^ generalized type
     , Exp TypecheckPhase )  -- ^ typechecked expression
typecheckExp e = (tcErrors, generalizedTyp, typechecked)
  where
    generalizedTyp =
      generalize (applySubst typ subst) (freeTyVarsGamma gamma)

    (tcErrors, ((typ, subst), typechecked)) = runTypechecker action

    action = do
      res <- solveSubproblem (tcExp gamma e)
      typechecked <- zonkExp e
      pure (res, typechecked)

    gamma = builtinsGamma

-- | evalExp maps a renamed expression to a type, in the monad
--   Typecheck. An additional pass turns a renamed expression with the
--   mapping generated by tcExp into a typechecked expression.
tcExp :: Gamma -> Exp RenamePhase -> Typecheck TcTyp
tcExp gamma (Located loc e) = go e
  where
    go :: Exp_ RenamePhase -> Typecheck TcTyp
    go (EInt i _) = do
      assignTypeOfTerm i intType
      pure intType
    go (EChar i _) = do
      assignTypeOfTerm i charType
      pure charType
    go (EString i _) = do
      assignTypeOfTerm i stringType
      pure stringType
    go (ENeg i (_, e)) = do
      tE <- tcExp gamma e
      eq tE intType i
      pure intType
    go (EVar i (Located _ (Ident _ id))) = do
      t <- tcVar i gamma id loc
      assignTypeOfTerm i t
      pure t
    go (EFun i (_, ps, _, body)) = do
      t <- tcFun i gamma ps body
      assignTypeOfTerm i t
      pure t
    go (EApp i (e, es)) = do
      tE <- tcExp gamma e
      tEs <- mapM (tcExp gamma) es
      tRet <- freshTyVar
      let tFun = funType tEs tRet
      eq tE tFun i
      assignTypeOfTerm i tRet
      pure tRet
    go (EBinop i (lhs, Located oploc (Op _ op), rhs)) = do
      tE <- tcVar i gamma op oploc
      tlhs <- tcExp gamma lhs
      trhs <- tcExp gamma rhs
      tRet <- freshTyVar
      let tFun = funType [tlhs, trhs] tRet
      eq tE tFun i
      assignTypeOfTerm i tRet
      pure tRet
    go (ELet i (_, decls, _, body)) = do
      gamma' <- tcDecls i gamma decls
      t <- tcExp gamma' body
      assignTypeOfTerm i t
      pure t
    go (ETuple i (_, es, _)) = do
      tEs <- mapM (tcExp gamma) (toList es)
      let t = tupType tEs
      assignTypeOfTerm i t
      pure t
    go (EList i (_, es, _)) = do
      tEs <- mapM (tcExp gamma) (toList es)
      tList <- freshTyVar
      mapM_ (\t -> eq t tList i) tEs
      let t = listType tList
      assignTypeOfTerm i t
      pure t
    go (EParens i (_, e, _)) = do
      t <- tcExp gamma e
      assignTypeOfTerm i t
      pure t
    go (EIf i (_, c, _, thenE, _, elseE)) = do
      tC <- tcExp gamma c
      tThenE <- tcExp gamma thenE
      tElseE <- tcExp gamma elseE
      eq tC boolType i
      eq tThenE tElseE i
      assignTypeOfTerm i tElseE
      pure tElseE
    go (ECase i (_, e, _, cases)) = do
      tE <- tcExp gamma e
      fresh <- freshTyVar
      tBs <- mapM (tcCase tE) cases
      mapM_ (\t -> eq t fresh i) tBs
      assignTypeOfTerm i fresh
      pure fresh
      where
        tcCase t (p, _, body) = do
          gamma' <- tcPattern i p t
          tcExp (gamma `Map.union` gamma') body
    go (EDo tid (_, stmts)) = do
      let (ds, e) = (init stmts, last stmts)
      gamma' <- foldlM (tcDoStmt tid) gamma ds
      case e of
        Located _ (DoExp _ e) -> do
          tE <- tcExp gamma' e
          fresh <- freshTyVar
          eq (ioType fresh) tE tid
          pure tE
        _ -> error "last stmt not expr"
    go (ETypArg _ _) = undefined
    go (EAnnot _ (e, _, _)) = undefined
    go (XExp _) = undefined

tcDoStmt
  :: TraceId
  -> Gamma
  -> DoStmt RenamePhase
  -> Typecheck Gamma
tcDoStmt tid gamma (Located loc stmt) = go stmt
  where
    go :: DoStmt_ RenamePhase -> Typecheck Gamma
    go (DoExp _ e) = do
      fresh <- freshTyVar
      tE <- tcExp gamma e
      eq (ioType fresh) tE tid
      pure gamma
    go (DoBind _ (p, _, e)) = do
      fresh <- freshTyVar
      tE <- tcExp gamma e
      eq (ioType fresh) tE tid
      gamma' <- tcPattern tid p fresh
      pure $ gamma `Map.union` gamma'
    go (DoLet _ (_, ds)) =
      tcDecls tid gamma ds
    go (XDo _) = undefined

tcFun
  :: TraceId
  -> Gamma
  -> [Pat RenamePhase]
  -> Exp RenamePhase
  -> Typecheck TcTyp
tcFun tid gamma ps body = do
  fresh <- mapM (const freshTyVar) (toList ps)
  gamma' <- Map.unions <$> zipWithM (tcPattern tid) ps fresh
  tBody <- tcExp (Map.union gamma' gamma) body
  pure $ funType fresh tBody

tcVar
  :: TraceId
  -> Gamma
  -> String
  -> Range
  -> Typecheck TcTyp
tcVar tid gamma nm range =
  case Map.lookup nm gamma of
    Just (TcForall alphas t) -> do
      let alphasList = IntSet.toList alphas
      fresh <- mapM (const freshTyVar) alphasList
      let t' = foldr
            (\(i, forT) t' -> substT i forT t') t
            (zip alphasList fresh)
      pure t'
    Nothing -> do
      addError $ UnboundVariableError tid nm range
      freshTyVar

isFunctionDecl :: Decl RenamePhase -> Bool
isFunctionDecl (Located _ (Equation _ (lhs, _, _))) =
  case lhs of
    (Right _) -> True
    (Left (Located _ (PVar _ (Located _ (Ident _ _))), _:_)) -> True
    _ -> False
isFunctionDecl _ = False

tcDecls :: TraceId -> Gamma -> [Decl RenamePhase] -> Typecheck Gamma
tcDecls tid gamma decls = foldlM handleGroup gamma groups
  where
    handleGroup :: Gamma -> [Decl RenamePhase] -> Typecheck Gamma
    handleGroup gamma [] = pure gamma
    handleGroup gamma ds@(d:_)
      | isFunctionDecl d = handleFunctionDeclGroup gamma ds
      | otherwise = handleBindingGroup gamma ds

    handleFunctionDeclGroup :: Gamma -> [Decl RenamePhase] -> Typecheck Gamma
    handleFunctionDeclGroup gamma ds = do
      fresh <- mapM (const freshTyVar) (toList ds)
      lambdas <- mapM asLambda ds
      let
        freshAndLambdas = zip lambdas fresh
        recGamma = foldl (\gamma' ((nm, _, _, _, _), alpha) ->
                             Map.insert nm (monotype alpha) gamma')
                   Map.empty freshAndLambdas
      (tys, sub) <- solveSubproblem $
        forM freshAndLambdas $ \((_, tid, ps, rhse, localGamma), alpha) -> do
        t <- tcFun tid (recGamma `Map.union` localGamma) ps rhse
        eq alpha t tid
        pure t

      let
        alphas = IntSet.intersection
          (domainSubst sub)
          (freeTyVarsGamma gamma)
        c' = map (\alpha -> Eq (TcVar alpha) (applySubst1 alpha sub) tid)
          (IntSet.toList alphas)
        boundSet =
          IntSet.union (freeTyVarsGamma gamma) (freeTyVarsConstraintSet c')
        schemes =
          map (\t -> generalize (applySubst t sub) boundSet) tys

      addConstraints c'
      pure
        $ Map.union gamma
        $ Map.fromList (zip (map nameOfLambda lambdas) schemes)

        where
          asLambda (Located _ (Equation tid (lhs, (_, rhse), maybeWhere))) = do
            localGamma <- case maybeWhere of
              Just (_, ds) -> tcDecls tid gamma ds
              Nothing -> pure gamma
            let (nm, ps) = nameAndPatternsOfLhs lhs
            pure (nm, tid, ps, rhse, localGamma)
          asLambda _ =
            error "non-function decl encountered in handleFunctionDeclGroup"

          nameOfLambda :: (String, a, b, c, d) -> String
          nameOfLambda (nm, _ ,_, _, _) = nm

    handleBindingGroup :: Gamma -> [Decl RenamePhase] -> Typecheck Gamma
    handleBindingGroup gamma [] = pure gamma
    handleBindingGroup gamma (d:ds) =
      case d of
        (Located _ (Equation tid (lhs, (_, rhse), maybeWhere))) -> do
          gamma' <- case maybeWhere of
            Just (_, ds) -> tcDecls tid gamma ds
            Nothing -> pure gamma

          case lhs of
            (Left (Located _ (PVar _ (Located _ (Ident _ nm))) , [])) -> do
              (rhst, sub) <- solveSubproblem $ tcExp gamma' rhse
              let
                alphas = IntSet.intersection
                  (domainSubst sub)
                  (freeTyVarsGamma gamma')
                c' = map (\alpha -> Eq (TcVar alpha) (applySubst1 alpha sub) tid)
                  (IntSet.toList alphas)
                boundSet =
                  IntSet.union (freeTyVarsGamma gamma') (freeTyVarsConstraintSet c')
                scheme = generalize (applySubst rhst sub) boundSet

              addConstraints c'
              pure $ Map.insert nm scheme gamma'
            -- Note: this TODO is mainly because it's not clear how to
            --       typecheck patterns when the rhs is polymorphic
            --       e.g.  let (a, b) = (int, \x -> x). This problem
            --       would be more easily solved if the above was
            --       desugared into:
            --       let tmp = (int, \x -> x)
            --           a = fst tmp
            --           b = snd tmp
            _ -> error "TODO: general pattern lhs in let binding"
        (Located _ (Signature _ _)) ->
          handleBindingGroup gamma ds
        (Located _ (XDecl _)) ->
          handleBindingGroup gamma ds

    groups = groupBy (\a b -> isFunctionDecl a && isFunctionDecl b) decls

tcPattern :: TraceId -> Pat RenamePhase -> TcTyp -> Typecheck Gamma
tcPattern tid (Located _ p) = go p
  where
    go :: Pat_ RenamePhase -> TcTyp -> Typecheck Gamma
    go (PInt _ _) t = do
      eq t intType tid
      pure Map.empty
    go (PVar _ (Located _ (Ident _ nm))) t = do
      pure $ Map.singleton nm (monotype t)
    go (PApp _ _) _ = undefined
    go (PTuple _ (_, ps, _)) t = do
      fresh <- mapM (const freshTyVar) (toList ps)
      eq t (tupType fresh) tid
      tPs <- zipWithM (tcPattern tid) (toList ps) fresh
      pure $ Map.unions tPs
    go (PList _ (_, ps, _)) t = do
      fresh <- freshTyVar
      eq t (listType fresh) tid
      tPs <- mapM (\p -> tcPattern tid p fresh) (toList ps)
      pure $ Map.unions tPs
    go (PCons _ (lhs, _, rhs)) t = do
      fresh <- freshTyVar
      eq t (listType fresh) tid
      tLhs <- tcPattern tid lhs fresh
      tRhs <- tcPattern tid rhs (listType fresh)
      pure $ Map.union tLhs tRhs
    go (PParens _ (_, p, _)) t = tcPattern tid p t
    go (PWild _ _) _ = pure Map.empty
    go (XPat _) _ = undefined
