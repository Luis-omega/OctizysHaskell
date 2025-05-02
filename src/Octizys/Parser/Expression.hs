{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Octizys.Parser.Expression where

import Control.Arrow ((<<<))
import qualified Data.Bifunctor
import Data.Char (isDigit)
import qualified Data.Foldable
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Effectful (Eff, (:>))
import Octizys.Cst.Expression
  ( Definition (Definition', definition, equal, name, outputType, parameters)
  , Expression
    ( Annotation
    , Application
    , EBool
    , EFunction
    , EInt
    , If
    , Let
    , Parens
    , Variable
    , applicationFunction
    , applicationRemain
    , colon
    , condition
    , definitions
    , expression
    , ifFalse
    , ifTrue
    , info
    , intValue
    , lparen
    , name
    , rparen
    , _else
    , _if
    , _in
    , _let
    , _then
    , _type
    )
  , Function (Function', arrow, body, parameters, start)
  , Parameter (Parameter', name, _type)
  , Parameters (Parameters', unParameters)
  )
import Octizys.Cst.InfoId (InfoId)
import Octizys.Cst.Type (Type)
import Octizys.Effects.Parser.Combinators
  ( many
  , optional
  , some
  , takeWhile1P
  , takeWhileP
  , try
  , (<?>)
  , (<|>)
  )
import Octizys.Effects.Parser.Effect (Parser, getParseState, putParseState)
import Octizys.Effects.SymbolResolution.Effect
  ( SymbolResolution
  , definitionOfExpressionVariable
  , foundExpressionVariable
  , removeExpressionDefinition
  , removeTypeDefinition
  )
import Octizys.Parser.Common
  ( OctizysParseError
  , between
  , comma
  , elseKeyword
  , identifierParser
  , ifKeyword
  , inKeyword
  , keyword
  , lambdaStart
  , leftParen
  , letKeyword
  , rightArrow
  , rightParen
  , semicolon
  , thenKeyword
  , tokenAndregister
  )
import qualified Octizys.Parser.Common as Common
import Octizys.Parser.Type (parseType, typeAtom)
import Prelude hiding (span)


parseExpression
  :: Parser OctizysParseError
    :> es
  => SymbolResolution :> es
  => Eff es Expression
parseExpression = expressionParser


-- ======================= Literals ===========================

boolParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Expression
boolParser =
  ((`EBool` True) <$> keyword "true")
    <|> ((`EBool` False) <$> keyword "false")


intParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Expression
intParser = do
  (value, inf) <-
    tokenAndregister
      ( do
          _head <- takeWhile1P (Just "digit") isDigit
          others <- takeWhileP (\c -> isDigit c || c == '_')
          pure (_head <> others)
      )
      <?> ('v' :| "alid integer")
  pure EInt {info = inf, intValue = value}


-- ======================= Expression ===========================

typeAnnotationParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es (InfoId, Type)
typeAnnotationParser = do
  colonInfo <- Common.colon
  _type <- parseType
  pure (colonInfo, _type)


variableParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Expression
variableParser = do
  (name, inf, _) <- identifierParser
  ei <- foundExpressionVariable name
  pure Variable {info = inf, name = ei}


parameterParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Parameter
parameterParser = do
  (nam, inf, parSpan) <- identifierParser
  maybeType <-
    optional
      ( do
          colonInfo <- Common.colon
          _type <- typeAtom
          pure (colonInfo, _type)
      )
  ei <- definitionOfExpressionVariable nam parSpan
  pure Parameter' {name = (inf, ei), _type = maybeType}


parametersParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es [(Parameter, InfoId)]
parametersParser = do
  originalState <- getParseState
  start <- parameterParser
  maybeInfoComma <- optional (try comma)
  case maybeInfoComma of
    Nothing -> do
      putParseState originalState
      pure []
    Just infoComma -> do
      remain <- parametersParser <|> pure []
      pure ((start, infoComma) : remain)


parameterOrParensParameter
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff
      es
      ( NonEmpty
          (Either (InfoId, Parameter, InfoId) Parameter)
      )
parameterOrParensParameter =
  some
    ( ( Left
          <$> between
            leftParen
            rightParen
            parameterParser
      )
        <|> Right
        <$> parameterParser
    )


maybeAnnotation
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Expression
maybeAnnotation = do
  expr <- parseExpression
  maybeType <- optional typeAnnotationParser
  case maybeType of
    Just (colonInfo, _type) ->
      pure Annotation {expression = expr, colon = colonInfo, _type}
    Nothing -> pure expr


parensExpressionParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Expression
parensExpressionParser = do
  (lparen, expression, rparen) <-
    between
      leftParen
      rightParen
      maybeAnnotation
  pure Parens {..}


removeParameters
  :: SymbolResolution :> es
  => [Parameter]
  -> Eff es ()
removeParameters = mapM_ removeParameter
  where
    removeParameter (Parameter' {_type = __type, name = _name}) =
      removeExpressionDefinition (snd _name)


functionParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Function
functionParser = do
  startInfo <- lambdaStart
  parameters <- parameterOrParensParameter
  arrowInfo <- rightArrow
  body <- expressionParser
  removeParameters
    ( either (\(_, p, _) -> p) id
        <$> NonEmpty.toList parameters
    )
  pure
    Function'
      { start = startInfo
      , parameters
      , arrow = arrowInfo
      , body
      }


atomExpressionParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Expression
atomExpressionParser =
  boolParser
    <|> intParser
    <|> parensExpressionParser
    -- Keep it at the end, it prevents the capture
    -- of keywords by variableParser
    -- Maybe we should check inside variableParser
    -- but this is a cheap trick
    <|> variableParser


ifParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Expression
ifParser = do
  _if <- ifKeyword
  condition <- expressionParser
  _then <- thenKeyword
  ifTrue <- expressionParser
  _else <- elseKeyword
  ifFalse <- expressionParser
  pure If {..}


removeDefinitions
  :: SymbolResolution :> es
  => NonEmpty Definition
  -> Eff es ()
removeDefinitions = mapM_ removeDefinition
  where
    removeDefinition (Definition' {name = _name}) =
      removeExpressionDefinition (snd _name)


-- TODO: handle undefinition of defined variable
-- so that the rest of the parser can
-- introduce a fresh name, consider :
-- let f x = g x
--     g x = f x
--
-- f and g should be visible in f and g bodies, but
-- x should be different for f and g
--
-- What happens if :
--
-- let f x = g x
--     g y = h y x
--     h w = w
--
-- The x in h souldn't be registered as the one in f.
definitionParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Definition
definitionParser = do
  (nam, inf, span) <- identifierParser
  ei <- definitionOfExpressionVariable nam span
  maybeParams <-
    optional
      ( do
          c <- Common.colon
          ps <- parametersParser <?> ('p' :| "arameter")
          pure (c, ps)
      )
  maybeOut <-
    optional
      ( case maybeParams of
          Just _ -> do
            commaInfo <- comma
            _type <- typeAtom
            pure (Just commaInfo, _type)
          Nothing -> do
            _type <- typeAtom
            pure (Nothing, _type)
      )
  eq <- Common.equal
  definition <- expressionParser
  let paramsAlone :: [Parameter] = maybe [] ((fst <$>) <<< snd) maybeParams
  removeParameters paramsAlone
  pure
    Definition'
      { name = (inf, ei)
      , definition
      , equal = eq
      , parameters = Data.Bifunctor.second Parameters' <$> maybeParams
      , outputType = maybeOut
      }


letParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Expression
letParser = do
  _let <- letKeyword
  definitions <-
    some
      ( do
          def <- definitionParser <?> ('a' :| " definition")
          semiInfo <- semicolon
          pure (def, semiInfo)
      )
  _in <- inKeyword
  expression <- parseExpression
  removeDefinitions (fst <$> definitions)
  pure Let {_let, definitions, _in, expression}


applicationParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Expression
applicationParser = do
  function <- atomExpressionParser
  -- the Try is a fix, if the parser of a identifier
  -- fails, then this will fail and we don't want that.
  arguments <- many (try atomExpressionParser)
  case arguments of
    [] -> pure function
    (ini : las) ->
      pure
        Application
          { applicationFunction = function
          , applicationRemain = ini :| las
          }


expressionParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Expression
expressionParser =
  ifParser
    <|> letParser
    <|> (EFunction <$> functionParser)
    <|> applicationParser
