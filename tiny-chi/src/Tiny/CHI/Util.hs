module Tiny.CHI.Util
  ( TransformOps(..)
  , transformAnno
  , transformExp
  , nameStream
  , nameAndPatternsOfLhs ) where

import Tiny.CHI.Anno
import Tiny.CHI.Types

data TransformOps a b m
  = TransformOps
  { t_ident      :: XIdent a -> m (XIdent b)
  , t_op         :: XOp a -> m (XOp b)
  , t_exp        :: XExp a -> m (XExp b)
  , t_xexp       :: XXExp a -> m (XXExp b)
  , t_pat        :: XPat a -> m (XPat b)
  , t_xpat       :: XXPat a -> m (XXPat b)
  , t_decl       :: XDecl a -> m (XDecl b)
  , t_xdecl      :: XXDecl a -> m (XXDecl b)
  , t_typ        :: XTyp a -> m (XTyp b)
  , t_xtyp       :: XXTyp a -> m (XXTyp b)
  , t_knd        :: XKnd a -> m (XKnd b)
  , t_xknd       :: XXKnd a -> m (XXKnd b)
  , t_do         :: XDo a -> m (XDo b)
  , t_xdo        :: XXDo a -> m (XXDo b)
  , t_topDecl    :: XTopDecl a -> m (XTopDecl b)
  , t_xtopDecl   :: XXTopDecl a -> m (XXTopDecl b)
  , t_sourceFile :: XSourceFile a -> m (XSourceFile b)
  }

transformAnno
  :: (Phase a, Phase b, Monad m)
  => TransformOps a b m
  -> SourceFile a -> m (SourceFile b)
transformAnno tops (SourceFile anno decls info) =
  SourceFile
  <$> t_sourceFile tops anno
  <*> mapM (transformTopDecl tops) decls
  <*> pure info

transformIdent
  :: (Phase a, Phase b, Monad m)
  => TransformOps a b m
  -> Ident a -> m (Ident b)
transformIdent tops (Located loc (Ident anno s)) =
  Located loc <$> (Ident <$> t_ident tops anno <*> pure s)

transformOp
  :: (Phase a, Phase b, Monad m)
  => TransformOps a b m
  -> Op a -> m (Op b)
transformOp tops (Located loc (Op anno s)) =
  Located loc <$> (Op <$> t_op tops anno <*> pure s)

transformExp
  :: (Phase a, Phase b, Monad m)
  => TransformOps a b m
  -> Exp a -> m (Exp b)
transformExp tops (Located loc e) = Located loc <$> go e
  where
    go (EInt anno i) = EInt <$> t_exp tops anno <*> pure i
    go (EChar anno d) = EChar <$> t_exp tops anno <*> pure d
    go (EString anno d) = EString <$> t_exp tops anno <*> pure d
    go (ENeg anno (t1, e)) =
      ENeg
      <$> t_exp tops anno
      <*> ((,) t1 <$> transformExp tops e)
    go (EVar anno v) = EVar <$> t_exp tops anno <*> transformIdent tops v
    go (EFun anno (t1, ps, t2, body)) =
      EFun
      <$> t_exp tops anno
      <*> ((,,,) t1 <$> mapM (transformPat tops) ps <*> pure t2 <*> transformExp tops body)
    go (EApp anno (e, es)) =
      EApp
      <$> t_exp tops anno <*> ((,) <$> transformExp tops e <*> mapM (transformExp tops) es)
    go (EBinop anno (lhs, op, rhs)) =
      EBinop
      <$> t_exp tops anno
      <*> ((,,) <$> transformExp tops lhs <*> transformOp tops op <*> transformExp tops rhs)
    go (ELet anno (t1, decls, t2, body)) =
      ELet
      <$> t_exp tops anno
      <*> ((,,,) t1 <$> mapM (transformDecl tops) decls <*> pure t2 <*> transformExp tops body)
    go (ETuple anno (t1, es, t2)) =
      ETuple
      <$> t_exp tops anno
      <*> ((,,) t1 <$> mapM (transformExp tops) es <*> pure t2)
    go (EList anno (t1, es, t2)) =
      EList
      <$> t_exp tops anno
      <*> ((,,) t1 <$> mapM (transformExp tops) es <*> pure t2)
    go (EParens anno (t1, e, t2)) =
      EParens
      <$> t_exp tops anno
      <*> ((,,) t1 <$> transformExp tops e <*> pure t2)
    go (EIf anno (t1, c, t2, thenE, t3, elseE)) =
      EIf
      <$> t_exp tops anno
      <*> ((,,,,,) t1
           <$> transformExp tops c
           <*> pure t2
           <*> transformExp tops thenE
           <*> pure t3
           <*> transformExp tops elseE)
    go (ECase anno (t1, e, t2, cases)) =
      ECase
      <$> t_exp tops anno
      <*> ((,,,) t1
           <$> transformExp tops e
           <*> pure t2
           <*> mapM (\(p, t, e) ->
                       (,,) <$> transformPat tops p <*> pure t <*> transformExp tops e) cases)
    go (EDo anno (t1, stmts)) =
      EDo
      <$> t_exp tops anno
      <*> ((,) t1 <$> mapM (transformDoStmt tops) stmts)
    go (ETypArg _ _) = error "impossible"
    go (EAnnot anno (e, t1, typ)) =
      EAnnot
      <$> t_exp tops anno
      <*> ((,,) <$> transformExp tops e <*> pure t1 <*> transformTyp tops typ)
    go (XExp anno) = XExp <$> t_xexp tops anno

transformTopDecl
  :: (Phase a, Phase b, Monad m)
  => TransformOps a b m
  -> TopDecl a -> m (TopDecl b)
transformTopDecl tops (Located loc d) = Located loc <$> go d
  where
    go (TypeDecl anno (t1, id, ids, t2, typ)) =
      TypeDecl
      <$> t_topDecl tops anno
      <*> ((,,,,) t1
           <$> transformIdent tops id
           <*> mapM (transformIdent tops) ids
           <*> pure t2
           <*> transformTyp tops typ)
    go (DataDecl _ _) = undefined
    go (Decl anno d) = Decl <$> t_topDecl tops anno <*> transformDecl tops d
    go (XTopDecl anno) = XTopDecl <$> t_xtopDecl tops anno

transformDecl
  :: (Phase a, Phase b, Monad m)
  => TransformOps a b m
  -> Decl a -> m (Decl b)
transformDecl tops (Located loc d) = Located loc <$> go d
  where
    go (Equation anno (lhs, rhs, maybeWhere)) =
      Equation
      <$> t_decl tops anno
      <*> ((,,) <$> renameLhs lhs <*> renameRhs rhs <*> renameWhere maybeWhere)
      where renameLhs (Left (p, ps)) =
              Left <$> ((,)
                        <$> transformPat tops p
                        <*> mapM (transformPat tops) ps)
            renameLhs (Right (l, op, r)) =
              Right <$> ((,,)
                         <$> transformPat tops l
                         <*> transformOp tops op
                         <*> transformPat tops r)
            renameRhs (t1, e) =
              (,) t1 <$> transformExp tops e
            renameWhere Nothing = pure Nothing
            renameWhere (Just (t1, ds)) =
              Just <$> ((,) t1 <$> mapM (transformDecl tops) ds)
    go (Signature _ _) = undefined
    go (XDecl anno) = XDecl <$> t_xdecl tops anno

transformDoStmt
  :: (Phase a, Phase b, Monad m)
  => TransformOps a b m
  -> DoStmt a -> m (DoStmt b)
transformDoStmt tops (Located loc s) = Located loc <$> go s
  where
    go (DoExp anno e) =
      DoExp <$> t_do tops anno <*> transformExp tops e
    go (DoBind anno (p, t1, e)) =
      DoBind
      <$> t_do tops anno
      <*> ((,,)
           <$> transformPat tops p
           <*> pure t1
           <*> transformExp tops e)
    go (DoLet anno (t1, ds)) =
      DoLet
      <$> t_do tops anno
      <*> ((,) t1 <$> mapM (transformDecl tops) ds)
    go (XDo anno) = XDo <$> t_xdo tops anno

transformPat
  :: (Phase a, Phase b, Monad m)
  => TransformOps a b m
  -> Pat a -> m (Pat b)
transformPat tops (Located loc p) = Located loc <$> go p
  where
    go (PInt anno i) = PInt <$> t_pat tops anno <*> pure i
    go (PVar anno v) =
      PVar <$> t_pat tops anno <*> transformIdent tops v
    go (PApp anno (p, ps)) =
      PApp
      <$> t_pat tops anno
      <*> ((,) <$> transformPat tops p <*> mapM (transformPat tops) ps)
    go (PTuple anno (t1, ps, t2)) =
      PTuple
      <$> t_pat tops anno
      <*> ((,,) t1 <$> mapM (transformPat tops) ps <*> pure t2)
    go (PList anno (t1, ps, t2)) =
      PList
      <$> t_pat tops anno
      <*> ((,,) t1 <$> mapM (transformPat tops) ps <*> pure t2)
    go (PCons anno (lhs, op, rhs)) =
      PCons
      <$> t_pat tops anno
      <*> ((,,)
           <$> transformPat tops lhs
           <*> transformOp tops op
           <*> transformPat tops rhs)
    go (PParens anno (t1, p, t2)) =
      PParens
      <$> t_pat tops anno
      <*> ((,,) t1 <$> transformPat tops p <*> pure t2)
    go (PWild anno t) = PWild <$> t_pat tops anno <*> pure t
    go (XPat anno) = XPat <$> t_xpat tops anno

transformTyp
  :: (Phase a, Phase b, Monad m)
  => TransformOps a b m
  -> Typ a -> m (Typ b)
transformTyp _ (Located _ _) = undefined

--------------------------------------------------------------------------------
-- Unique Names

crossN :: Int -> [a] -> [[a]]
crossN 1 as = map (:[]) as
crossN n as = as >>= \a -> map (a:) (crossN (n - 1) as)

nameStream :: [String]
nameStream = go 1
  where
    go gen = crossN gen ['a'..'z'] ++ go (gen + 1)

nameAndPatternsOfLhs :: LHS a -> (String, [Pat a])
nameAndPatternsOfLhs (Left (Located _ (PVar _ (Located _ (Ident _ name))), ps)) =
  (name, ps)
nameAndPatternsOfLhs (Right (lp, Located _ (Op _ op), rp)) =
  (op, [lp, rp])
nameAndPatternsOfLhs _ =
  error "non-function lhs encountered in handleFunctionDeclGroup"
