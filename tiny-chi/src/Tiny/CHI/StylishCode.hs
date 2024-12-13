{-# LANGUAGE MultiParamTypeClasses #-}

module Tiny.CHI.StylishCode where

import Control.Monad.State
import Data.List
import StylishText
import StylishText.Utils
import Tiny.CHI.Anno
import Tiny.CHI.Types
import Control.Applicative
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Misc utils

doubleton x y = [x, y]

cons2 x y zs  = x:y:zs

uncurry3 f (a,b,c) = f a b c

mapSnd3 f (a,b,c) = (a, f b, c)

snoc xs x = xs ++ [x]

--------------------------------------------------------------------------------
-- State Pos monad

-- sequenceA @[] @(State Pos) @StylishText
--  :: [State Pos StylishText] -> State Pos [StylishText]

-- traverse @[] @(State Pos)
--   :: (a -> State Pos b) -> [a] -> State Pos [b]

data ContentAfter x
  = ContentAfter
  { content_after_exp :: XExp x -> Maybe StylishText
  , content_after_do_stmt :: XDo x -> Maybe StylishText
  }

data StylishCodeState x
  = StylishCodeState
  { scs_pos :: Pos
  , scs_content_after :: ContentAfter x
  }

class StylishCode t x where
  emitStylish
    :: SourceFileInfo
    -> (Path, t)
    -> State (StylishCodeState x) StylishText

-- Note: A very modest optimization might be to put the CommentBox
-- in the State type, removing comments as they are rendered. Would
-- also keep the comments (the Ranges) in an ascending list for
-- faster retrieval. But overall, unlikely to be worth it.

incrementPosWithLeaf str pos@(Pos r c off)
  | str == ""     = pos
  | null newlines = Pos r (c + length str) (off + length str)
  | otherwise     = Pos (r + length newlines)
                         (1 + length (drop (last newlines + 1) str))
                         (off + length str)
      where
        newlines = elemIndices '\n' str

emitLeafAndMove
  :: Phase x
  => Maybe Path
  -> Classes
  -> Styles
  -> String
  -> State (StylishCodeState x) StylishText
emitLeafAndMove mp cs styles str = state $ \(StylishCodeState pos0 trans) ->
  ( Node mp cs styles [TextLeaf str]
  , StylishCodeState (incrementPosWithLeaf str pos0) trans)

emitWhitespaceLeaf
  :: Phase x
  => String
  -> State (StylishCodeState x) StylishText
emitWhitespaceLeaf = emitLeafAndMove Nothing [] [("white-space", "pre", 0)]

emitLeaf
  :: Phase x
  => ClassName
  -> Pathed String -> State (StylishCodeState x) StylishText
emitLeaf tag (path, str) = emitLeafAndMove (Just path) [tag] [] str

emitNode
  :: (Phase x, StylishCode a x)
  => SourceFileInfo
  -> ClassName
  -> Path
  -> Pathed a
  -> State (StylishCodeState x) StylishText
emitNode env tag path tuple =
  Node (Just path) [tag] [] <$> singleton <$> emitStylish env tuple

emitNodeExp
  :: (Phase x, StylishCode a x)
  => SourceFileInfo
  -> ClassName
  -> Path
  -> XExp x
  -> Pathed a
  -> State (StylishCodeState x) StylishText
emitNodeExp env tag path anno tuple = do
  contentAfter <- gets (($ anno) . content_after_exp . scs_content_after)
  let inject a = maybe [a] (([a]++) . singleton) contentAfter
  Node (Just path) [tag] [] <$> inject <$> emitStylish env tuple

emitNodeDo
  :: (Phase x, StylishCode a x)
  => SourceFileInfo
  -> ClassName
  -> Path
  -> XDo x
  -> Pathed a
  -> State (StylishCodeState x) StylishText
emitNodeDo env tag path anno tuple = do
  contentAfter <- gets (($ anno) . content_after_do_stmt . scs_content_after)
  let inject a = maybe [a] (([a]++) . singleton) contentAfter
  Node (Just path) [tag] [] <$> inject <$> emitStylish env tuple

emitSpaces
  :: Phase x
  => Int
  -> State (StylishCodeState x) StylishText
emitSpaces n = emitWhitespaceLeaf $ replicate n ' '

emitBreaks
  :: Phase x
  => Int
  -> State (StylishCodeState x) StylishText
emitBreaks n = emitWhitespaceLeaf $ replicate n '\n'

emitNothing :: Phase x => State (StylishCodeState x) StylishText
emitNothing = emitSpaces 0

--------------------------------------------------------------------------------

instance (Phase x, StylishCode a x) => StylishCode (Located a) x where
  emitStylish env arg =
    let ((_,range), a) = subvaluesLocated arg in
    liftA2 prependWhitespace
      (emitWhitespaceTo env Nothing $ start range)
      (emitStylish env a)

subvaluesLocated :: Pathed (Located a) -> (Pathed Range, Pathed a)
subvaluesLocated x@(_, Located _1 _2) = subvalues2 $ konst (_1,_2) x

emitWhitespaceTo
  :: Phase x
  => SourceFileInfo
  -> Maybe String
  -> Pos
  -> State (StylishCodeState x) [StylishText]
emitWhitespaceTo env@(_, commentBox) maybeDebugStr target@(Pos r2 c2 _) = do
  current@(Pos r1 c1 _) <- gets scs_pos
  if r1 > r2 || (r1 == r2 && c1 > c2) then
    error $
      "emitWhitespace from " ++ show current ++ " to " ++ show target ++
      "[" ++ show maybeDebugStr ++ "]"
  else if r1 == r2 && c1 == c2 then
    pure []
  else
    case commentsInRange commentBox (Range current target) of
      [] ->
        -- TODO: Think about whether we really want/need to
        -- explicitly separate breaks and spaces.
        if r1 == r2 then
          singleton <$> emitSpaces (c2-c1)
        else
          doubleton <$> emitBreaks (r2-r1) <*> emitSpaces (c2-1)
      ranges ->
        liftA2 (++)
          (concat <$> (sequenceA $ map (emitComment env) ranges))
          (emitWhitespaceTo env Nothing target)

rangeContainedIn :: Range -> Range -> Bool
rangeContainedIn outer inner =
  start outer <= start inner && end inner <= end outer

commentsInRange :: CommentBox -> Range -> [Range]
commentsInRange commentBox outer =
  Set.toAscList $ Set.filter (rangeContainedIn outer) commentBox

emitComment
  :: Phase x
  => SourceFileInfo
  -> Range
  -> State (StylishCodeState x) [StylishText]
emitComment env@(sourceText, _) range =
  liftA2 snoc
    (emitWhitespaceTo env Nothing (start range))
    (emitWhitespaceLeaf (sliceAndUnpack range sourceText))

prependWhitespace :: [StylishText] -> StylishText -> StylishText
prependWhitespace [] node = node
prependWhitespace wsLeaves node@(Node _ _ _ _) =
  Node Nothing [] [] $ wsLeaves ++ [node]
prependWhitespace _ _ = undefined

--------------------------------------------------------------------------------
-- Boilerplate

konst = fmap . const

-- | skip over the annotation field
subvals :: Pathed a -> Pathed a
subvals (p, x0) = (p++[1], x0)

instance Phase x => StylishCode (Ident_ x) x where
  emitStylish _ (path, Ident _ str) = emitLeaf "ident" (path, str)

instance Phase x => StylishCode (Op_ x) x where
  emitStylish _ (path, Op _ op) = emitLeaf "op" (path, op)

instance Phase x => StylishCode (Token_ tok) x where
  emitStylish _ (path, Token str) = emitLeaf "token" (path, str)

instance Phase x => StylishCode Char x where
  emitStylish _ (p, c) = emitLeaf "char" (p, [c])

instance Phase x => StylishCode (Exp_ x) x where
  emitStylish _     (p, EInt    _     i) = emitLeaf "exp" (p, show i) -- TODO: this should also process anno
  emitStylish env v@(p, EChar   anno _1) = emitNodeExp env "exp" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, EString anno _1) = emitNodeExp env "exp" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, ENeg    anno _1) = emitNodeExp env "exp" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, EVar    anno _1) = emitNodeExp env "exp" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, EFun    anno _1) = emitNodeExp env "exp" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, EApp    anno _1) = emitNodeExp env "exp" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, EBinop  anno _1) = emitNodeExp env "exp" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, ELet    anno _1) = emitNodeExp env "exp" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, EList   anno _1) = emitNodeExp env "exp" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, ETuple  anno _1) = emitNodeExp env "exp" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, EParens anno _1) = emitNodeExp env "exp" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, EIf     anno _1) = emitNodeExp env "exp" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, ECase   anno _1) = emitNodeExp env "exp" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, EDo     anno _1) = emitNodeExp env "exp" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, ETypArg anno _1) = emitNodeExp env "exp" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, EAnnot  anno _1) = emitNodeExp env "exp" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, XExp _) = undefined

instance Phase x => StylishCode (Pat_ x) x where
  emitStylish _     (p, PInt    _  i) = emitLeaf "pat" (p, show i)
  emitStylish env v@(p, PVar    _ _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PApp    _ _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PTuple  _ _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PList   _ _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PCons   _ _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PParens _ _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, PWild   _ _1) = emitNode env "pat" p $ subvals $ konst _1 v
  emitStylish env v@(p, XPat _) = undefined

instance Phase x => StylishCode (Decl_ x) x where
  emitStylish env v@(p, Equation  _ _1) = emitNode env "decl" p $ subvals $ konst _1 v
  emitStylish env v@(p, Signature _ _1) = emitNode env "decl" p $ subvals $ konst _1 v
  emitStylish env v@(p, XDecl _) = undefined

instance Phase x => StylishCode (TopDecl_ x) x where
  emitStylish env v@(p, TypeDecl  _ _1) = emitNode env "top-decl" p $ subvals $ konst _1 v
  emitStylish env v@(p, DataDecl  _ _1) = emitNode env "top-decl" p $ subvals $ konst _1 v
  emitStylish env v@(p, Decl      _ _1) = emitNode env "top-decl" p $ subvals $ konst _1 v
  emitStylish env v@(p, XTopDecl _) = undefined

instance Phase x => StylishCode (DoStmt_ x) x where
  emitStylish env v@(p, DoExp  anno _1) = emitNodeDo env "do-stmt" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, DoBind anno _1) = emitNodeDo env "do-stmt" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, DoLet  anno _1) = emitNodeDo env "do-stmt" p anno $ subvals $ konst _1 v
  emitStylish env v@(p, XDo _) = undefined

instance Phase x => StylishCode (Typ_ x) x where
  emitStylish env v@(p, TUnit      _ _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TCon       _ _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TVar       _ _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TFun       _ _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TFunCon    _ _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TApp       _ _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TList      _ _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TListCon   _ _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TTuple     _ _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TTupleCon  _ _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TParens    _ _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TForall    _ _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TContxt    _ _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, TAnnot     _ _1) = emitNode env "typ" p $ subvals $ konst _1 v
  emitStylish env v@(p, XTyp _) = undefined

instance Phase x => StylishCode (Knd_ x) x where
  emitStylish env v@(p, KStar   _ _1) = emitNode env "knd" p $ subvals $ konst _1 v
  emitStylish env v@(p, KFun    _ _1) = emitNode env "knd" p $ subvals $ konst _1 v
  emitStylish env v@(p, KFunCon _ _1) = emitNode env "knd" p $ subvals $ konst _1 v
  emitStylish env v@(p, KApp    _ _1) = emitNode env "knd" p $ subvals $ konst _1 v
  emitStylish env v@(p, KParens _ _1) = emitNode env "knd" p $ subvals $ konst _1 v
  emitStylish env v@(p, XKnd _) = undefined

--------------------------------------------------------------------------------

-- TODO: For primitive tuples and lists, we may want to add pathedNodes.
-- Otherwise, matching those subvalues will not be connected to stylish text.

instance Phase x => StylishCode () x where
  emitStylish _ _ = emitNothing

instance (Phase x, StylishCode a x) => StylishCode [a] x where
  emitStylish env arg =
    Node Nothing [] [] <$> (sequenceA $ map (emitStylish env) $ subvaluesN arg)

instance (Phase x, StylishCode a x) => StylishCode (DelimitedList tok a) x where
  emitStylish env v@(_, DelimitedList _1) =
    emitStylish env $ subvalues1 $ konst _1 v

instance (Phase x, StylishCode a x) => StylishCode (DelimitedNonEmptyList tok a) x where
  emitStylish env v@(_, DelimitedNonEmptyList _1) =
    emitStylish env $ subvalues1 $ konst _1 v

instance (Phase x, StylishCode a x) => StylishCode (Maybe a) x where
  emitStylish _     (_, Nothing) = emitNothing
  emitStylish env v@(_, Just _1) = emitStylish env $ subvalues1 $ konst _1 v

instance (Phase x, StylishCode a x, StylishCode b x) => StylishCode (Either a b) x where
  emitStylish env v@(_, Left  _1) = emitStylish env $ subvalues1 $ konst _1 v
  emitStylish env v@(_, Right _1) = emitStylish env $ subvalues1 $ konst _1 v

instance (Phase x, StylishCode a1 x, StylishCode a2 x) => StylishCode (a1, a2) x where
  emitStylish env arg =
    let (_1, _2) = subvalues2 arg in
    Node Nothing [] [] <$> (sequenceA [emitStylish env _1, emitStylish env _2])

instance (Phase x, StylishCode a1 x, StylishCode a2 x, StylishCode a3 x) => StylishCode (a1, a2, a3) x where
  emitStylish env arg =
    let (_1, _2, _3) = subvalues3 arg in
    Node Nothing [] [] <$> (sequenceA [emitStylish env _1, emitStylish env _2, emitStylish env _3])

instance (Phase x, StylishCode a1 x, StylishCode a2 x, StylishCode a3 x, StylishCode a4 x) => StylishCode (a1, a2, a3, a4) x where
  emitStylish env arg =
    let (_1, _2, _3, _4) = subvalues4 arg in
    Node Nothing [] [] <$> (sequenceA [emitStylish env _1, emitStylish env _2, emitStylish env _3, emitStylish env _4])

instance (Phase x, StylishCode a1 x, StylishCode a2 x, StylishCode a3 x, StylishCode a4 x, StylishCode a5 x) => StylishCode (a1, a2, a3, a4, a5) x where
  emitStylish env arg =
    let (_1, _2, _3, _4, _5) = subvalues5 arg in
    Node Nothing [] [] <$> (sequenceA [emitStylish env _1, emitStylish env _2, emitStylish env _3, emitStylish env _4, emitStylish env _5])

instance (Phase x, StylishCode a1 x, StylishCode a2 x, StylishCode a3 x, StylishCode a4 x, StylishCode a5 x, StylishCode a6 x) => StylishCode (a1, a2, a3, a4, a5, a6) x where
  emitStylish env arg =
    let (_1, _2, _3, _4, _5, _6) = subvalues6 arg in
    Node Nothing [] [] <$> (sequenceA [emitStylish env _1, emitStylish env _2, emitStylish env _3, emitStylish env _4, emitStylish env _5, emitStylish env _6])
