{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Stylesheet.Types where

import           Control.Monad
import           Control.Monad.State (StateT)
import           Data.List
import           Data.Map            (Map)
import           Data.Set            (Set)
import           Language.Haskell.TH
import           Language.Haskell.TH.Lift
import qualified Control.Monad.State as S
import qualified Data.Map as Map
import qualified Data.Set as Set

import StylishText (TargetedStyles)

type Classes   = [ClassName]
type ClassName = String
type Var       = String

type AtBinding = Maybe Var -- x@(...)

data Pattern
  = PVar String
  | PWild
  | PKeepout
  | PConstructor AtBinding String [TypeSig] [Pattern]
  | PTuple [Pattern]
  | PChar Char
  | PString String
  | PSig Pattern TypeSig
  deriving Show

newtype TypeSig
  = TCon String
  deriving Show

fvPattern :: Pattern -> Set String
fvPattern (PVar v) = Set.singleton v
fvPattern PWild = Set.empty
fvPattern PKeepout = Set.empty
fvPattern (PConstructor (Just v) _ _ ps) =
  foldl Set.union (Set.singleton v) (map fvPattern ps)
fvPattern (PConstructor Nothing  _ _ ps) =
  foldl Set.union Set.empty (map fvPattern ps)
fvPattern (PTuple ps) =
  foldl Set.union Set.empty (map fvPattern ps)
fvPattern (PChar _) = Set.empty
fvPattern (PString _) = Set.empty
fvPattern (PSig p _) = fvPattern p

-- tiny language of expressions in order to
-- write predicates
data PredExp
  = PEVar String
  | PEApp PredExp PredExp
  deriving Show

data PrimitiveSelector
  = PatternSelector AtBinding Pattern [String]
  | ClassSelector AtBinding [String]
  deriving Show

fvPrimitiveSelector :: PrimitiveSelector -> Set String
fvPrimitiveSelector (PatternSelector (Just v) pat _) =
  Set.insert v (fvPattern pat)
fvPrimitiveSelector (PatternSelector Nothing  pat _) = fvPattern pat
fvPrimitiveSelector (ClassSelector (Just v) _) = Set.singleton v
fvPrimitiveSelector (ClassSelector Nothing  _) = Set.empty

data StyleAttrs
  = StyleAttrs
  { rule_vars::[Var]
  , rule_properties::[(String, String, Int)]
  }
  deriving Show

type StyleAttrsSet = [StyleAttrs]

data SelectorP = SelectorP [PrimitiveSelector] (Maybe PredExp)
  deriving Show

data Rule = Rule [SelectorP] StyleAttrsSet
  deriving Show

type Stylesheet = [Rule]

instance Lift Rule where
  liftTyped = unsafeCodeCoerce . lift
  lift = ruleToQRule

type Path = [Int]

type BoundVars = [(String, (Name, Path))]

data LiftSelectorState
  = LiftSelectorState
  { -- lps_bound_vars maps user-given names to template
    -- Haskell unique (unable to be captured) names,
    -- and the variable's path within the pattern.
    lps_bound_vars   ::[(String, (Name, Path))]
  , lps_keepout_paths::[Path]
  }

type S m a = StateT LiftSelectorState m a

iforM :: (Monad m) => [a] -> (Int -> a -> m b) -> m [b]
iforM as f = zipWithM f [0..] as

-- Returns a fresh Name and updates the state so that
-- nm is bound to this VarP at the current path.
bindVariable :: Quote m => String -> Path -> S m Name
bindVariable v p = do
  nm <- S.lift (newName v)
  hasBinding <- S.gets (any ((==v) . fst) . lps_bound_vars)
  when hasBinding $ error  (v ++ " is already bound!")
  S.modify (\s -> s { lps_bound_vars = (v, (nm, p)):lps_bound_vars s })
  pure nm

addKeepoutPath :: Quote m => Path -> S m ()
addKeepoutPath p =
  S.modify (\s -> s { lps_keepout_paths = p:lps_keepout_paths s })

wrapInAt :: Quote m => Var -> Pat -> Path -> S m Pat
wrapInAt v pat p = AsP <$> bindVariable v p <*> pure pat

typeSigToQType :: Quote m => TypeSig -> S m Type
typeSigToQType (TCon c) = pure $ ConT (mkName c)

patToQPat :: Quote m => Pattern -> Path -> S m Pat
patToQPat (PVar v) p = VarP <$> bindVariable v p
patToQPat PWild _ = pure WildP
patToQPat PKeepout p = addKeepoutPath p >> pure WildP
patToQPat (PConstructor Nothing con tys pats) p =
  ConP (mkName con)
  <$> mapM typeSigToQType tys
  <*> iforM pats (\idx pat -> patToQPat pat (p ++ [idx]))
patToQPat (PConstructor (Just v) con tys pats) p = do
  c <- ConP (mkName con)
    <$> mapM typeSigToQType tys
    <*> iforM pats (\idx pat -> patToQPat pat (p ++ [idx]))
  wrapInAt v c p
patToQPat (PTuple pats) p =
  TupP <$> iforM pats (\idx pat -> patToQPat pat (p ++ [idx]))
patToQPat (PChar c) _ =
  pure (LitP (CharL c))
patToQPat (PString s) _ =
  pure (LitP (StringL s))
patToQPat (PSig pat t) p =
  SigP <$> patToQPat pat p <*> typeSigToQType t

(!) :: Eq a => [(a, b)] -> a -> b
[] ! _ = error "Not found!"
((a, b):rest) ! a'
  | a == a' = b
  | otherwise = rest ! a'

primSelectorToQPatSelector
  :: Quote m => PrimitiveSelector
  -> S m Exp
primSelectorToQPatSelector selector@(PatternSelector at pat classes) = do
  qPat <- case at of
    Nothing -> patToQPat pat []
    Just v -> do
         p <- patToQPat pat []
         wrapInAt v p []
  boundVars <- S.gets lps_bound_vars
  let nothing = NormalB (ConE (mkName "Nothing"))
      just a = NormalB (AppE (ConE (mkName "Just")) a)
      binds = just $ ListE
              $ map (AppE (ConE (mkName "Binding")) . VarE . fst . (boundVars !))
              $ Set.toList
              $ fvPrimitiveSelector selector
      -- \case { qPat -> Just bindings; _ -> Nothing }
      lambda = LamCaseE [Match qPat binds [], Match WildP nothing []]

  classesE <- S.lift [e| classes |]

  -- PatternSelector (select lambda) classes
  pure $ AppE
    (AppE
      (AppE (ConE (mkName "PatternSelector")) (LitE (IntegerL (-1))))
      (AppE (VarE (mkName "select")) lambda))
    classesE
primSelectorToQPatSelector (ClassSelector at classes) = do
  case at of
    Nothing -> pure ()
    Just v  -> void $ bindVariable v []
  classesE <- S.lift [e| classes |]
  pure $ AppE
    (AppE
     (ConE (mkName "ClassSelector")) (LitE (IntegerL (-1)))) classesE

constructTargetedStyles
  :: PrimitiveSelector
  -> LiftSelectorState
  -> Map Var [(String, String, Int)]
  -> TargetedStyles
constructTargetedStyles selector state propertiesMap = targetedStyles
  where fvs = fvPrimitiveSelector selector
        mentionedVars =
          Map.toList
          $ Map.filterWithKey (\v _ -> Set.member v fvs) propertiesMap
        targetedStyles =
          map (\(k, v) ->
                 let path = snd $ lps_bound_vars state ! k
                 in (path, v)) mentionedVars

selectorToExp :: Quote m => [PrimitiveSelector] -> StyleAttrsSet -> m (Exp, BoundVars)
selectorToExp primSelectors ruleSet = do
  (primSelectorExprs, finalState) <- S.runStateT
    (mapM primSelectorToQPatSelector primSelectors)
    (LiftSelectorState [] [])

  -- sort boundVars by name so that the bindings are in the same order
  -- as their instantiation.
  let boundVars = sortOn fst (lps_bound_vars finalState)

    -- create a map from bound var to list of style properties
  let boundVarToProperties = foldl
        (\m (StyleAttrs vars properties) ->
           Map.unionWith (++) m $ Map.fromList (map (, properties) vars) )
        Map.empty ruleSet

  primSelectorRuleEs <- zipWithM
    (\selector selectorE ->
       do let targetedStyles = constructTargetedStyles
                selector
                finalState
                boundVarToProperties
          let keepOutPaths = lps_keepout_paths finalState
          targetedStylesE <- [e| targetedStyles |]
          keepOutPathsE <- [e| keepOutPaths |]
          pure $ RecConE
            (mkName "PrimitiveSelectorRule")
            [ (mkName "targetedStyles", targetedStylesE)
            , (mkName "keepOutPaths", keepOutPathsE)
            , (mkName "selector", selectorE) ]
    )
    primSelectors primSelectorExprs

  pure (ListE primSelectorRuleEs, boundVars)

selectorPToExp :: Quote m => SelectorP -> StyleAttrsSet -> m Exp
selectorPToExp (SelectorP primSelectors maybePredicate) ruleSet = do
  (primSelectorRulesE, boundVars) <- selectorToExp primSelectors ruleSet

  let
    false = NormalB (ConE (mkName "False"))
    -- given nm, make Binding nm pattern
    bindingP nm = conP (mkName "Binding") [varP (mkName nm)]

    -- [Binding nm1, Binding nm2, ...]
    bindersP = listP $ map (bindingP . fst) boundVars

    liftPredE (PEApp l r) = appE (liftPredE l) (liftPredE r)
    liftPredE (PEVar nm) = varE . mkName $ nm

  predExp <- case maybePredicate of
    Just predicate -> do
      binders <- bindersP
      predicateBody <- liftPredE predicate

      -- \case { [Binding nm1, Binding nm2, ...] -> pred; _ -> False }
      pure $ LamCaseE
             [ Match binders (NormalB predicateBody) []
             , Match WildP false [] ]

    Nothing -> [e| const True |]

  pure $ AppE (AppE (ConE (mkName "Rule")) primSelectorRulesE) predExp

ruleToQRule :: Quote m => Rule -> m Exp
ruleToQRule (Rule alternatives ruleSet) =
  ListE <$> mapM (`selectorPToExp` ruleSet) alternatives
