{-# LANGUAGE OverloadedStrings #-}

module Stylesheet.Parser (parseRule, parseStylesheet) where

import Control.Applicative hiding ( many, some )
import Control.Monad
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Stylesheet.Types

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockCommentNested "{-" "-}") <?> "whitespace"

-- Consume trailing whitespace after a parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Match the given text and then consume trailing whitespace
symbol :: Text -> Parser Text
symbol = L.symbol sc

parenthesized :: Parser a -> Parser a
parenthesized p = hidden (symbol "(") >> p <* hidden (symbol ")")

pConstructor :: Parser String
pConstructor = liftA2 (:)
  C.upperChar
  (many (C.letterChar
         <|> C.numberChar
         <|> C.char '_'
         <|> C.char '\'')) <?> "constructor"

pVariable :: Parser String
pVariable = liftA2 (:)
  C.lowerChar
  (many (C.letterChar
        <|> C.numberChar
        <|> C.char '_'
        <|> C.char '\'')) <?> "variable"

pClassName :: Parser ClassName
pClassName = liftA2 (:)
  beginChar
  (many (beginChar <|> C.numberChar)) <?> "class name"
  where beginChar =
          C.letterChar
          <|> C.char '_'
          <|> C.char '-'

pPropertyKey :: Parser String
pPropertyKey = pClassName <?> "property key"

pPropertyValue :: Parser String
pPropertyValue =
  some (satisfy (`notElem` [';', '!']))

pCharLit :: Parser Char
pCharLit = between (char '\'') (char '\'') L.charLiteral

pStringLit :: Parser String
pStringLit = char '\"' *> manyTill L.charLiteral (char '\"')

pDontCare :: Parser ()
pDontCare = void $ C.char '_'

pAtBind :: Parser Var
pAtBind = lexeme pVariable <* symbol "@"

-- two or more patterns, to disambiguate from parenthesization
pTuplePattern :: Parser Pattern
pTuplePattern = do
  a <- pPattern
  void $ symbol ","
  b <- pPattern
  PTuple . ([a, b]++) <$> many (symbol "," >> pPattern)

pTypeSig :: Parser TypeSig
pTypeSig = TCon <$> pConstructor

pTypeApp :: Parser TypeSig
pTypeApp = symbol "@" >> lexeme pTypeSig

pPattern0 :: Parser Pattern
pPattern0 = (PWild <$ lexeme pDontCare)
            <|> (PKeepout <$ void (symbol "xxx"))
            <|> try (PConstructor
                     <$> optional pAtBind
                     <*> lexeme pConstructor
                     <*> many pTypeApp
                     <*> many pPattern)
            <|> (PVar <$> pVariable)
            <|> try (parenthesized pTuplePattern <?> "tuple pattern")
            <|> parenthesized pPattern
            <|> (PChar <$> pCharLit)
            <|> (PString <$> pStringLit)

pPattern :: Parser Pattern
pPattern = do
  p <- pPattern0
  sig <- optional (symbol "::" >> pTypeSig)
  pure $ case sig of
    Just typ -> PSig p typ
    Nothing -> p

pPatternSelector :: Parser PrimitiveSelector
pPatternSelector = PatternSelector
                   <$> optional pAtBind
                   <*> pPattern
                   <*> many (C.char '.' >> pClassName)

pClassSelector :: Parser PrimitiveSelector
pClassSelector = ClassSelector
                 <$> optional pAtBind
                 <*> some (C.char '.' >> pClassName)

pPrecedenceOverride :: Parser Int
pPrecedenceOverride = symbol "!" >> L.decimal

pProperty :: Parser (String, String, Int)
pProperty = (,,)
            <$> (lexeme pPropertyKey   <* symbol ":")
            <*> lexeme pPropertyValue
            <*> lexeme (fromMaybe 0 <$> optional pPrecedenceOverride)
            <*  symbol ";"

pStyleAttrs :: Parser StyleAttrs
pStyleAttrs = StyleAttrs
        <$> manyTill (pVariable <* (void (symbol ",") <|> sc)) (symbol "{")
        <*> many (lexeme pProperty)
        <*  symbol "}"

pPredicate :: Parser (Maybe PredExp)
pPredicate = optional (symbol "if" >> pExp <* sc)
  where pExp :: Parser PredExp
        pExp = foldl1 PEApp <$> some (lexeme pTerm)

        pTerm :: Parser PredExp
        pTerm = choice
          [ between (symbol "(") (symbol ")") pExp
          , PEVar <$> pVariable
          ]

pPrimSelectors :: Parser [PrimitiveSelector]
pPrimSelectors = sepEndBy1
  (try (parenthesized pPatternSelector)
    <|> try (parenthesized pClassSelector)
    <|> try pPatternSelector
    <|> try pClassSelector)
  sc

pSelectorP :: Parser SelectorP
pSelectorP = SelectorP
             <$> pPrimSelectors
             <*> pPredicate

pRule :: Parser Rule
pRule = sc >> Rule
        <$> sepEndBy1 pSelectorP (symbol ",")
        <*  symbol "->"
        <*> some pStyleAttrs

pStylesheet :: Parser Stylesheet
pStylesheet = many pRule

parseRule :: SourcePos -> Text -> Either String Rule
parseRule pos text =
  let initState = State
        { stateInput = text
        , stateOffset = 0
        , statePosState = PosState
          { pstateInput = text
          , pstateOffset = 0
          , pstateSourcePos = pos
          , pstateTabWidth = defaultTabWidth
          , pstateLinePrefix = ""
          }
        , stateParseErrors = []
        }
      outputOrError = snd $ runParser' (pRule <* eof) initState
  in case outputOrError of
    Left err -> Left $ errorBundlePretty err
    Right selector -> Right selector

parseStylesheet :: Text -> Either String Stylesheet
parseStylesheet text = case parse (pStylesheet <* eof) "" text of
  Left err -> Left $ errorBundlePretty err
  Right stylesheet -> Right stylesheet
