{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Octizys.Parser.Expression where

import Control.Monad (join)
import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Effectful (Eff, (:>))
import Octizys.Cst.Expression
  ( Definition
      ( Definition'
      , colon
      , definition
      , equal
      , name
      , outputType
      , parameters
      )
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
  , FunctionParameter
    ( FunctionParameterAlone
    , FunctionParameterWithType
    , parameter
    )
  , Parameter (ParameterAlone, ParameterWithType, colon, name, _type)
  , Parameters (Parameters')
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
import Octizys.Parser.Type (parseType, typeAtom, typeAtomNoVar)
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
  pure Parens {lparen, rparen, expression}


atomExpressionParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Expression
atomExpressionParser =
  boolParser
    <|> intParser
    <|> parensExpressionParser
    <|> variableParser


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


parameterAlone
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Parameter
parameterAlone = do
  (nam, inf, parSpan) <- identifierParser
  ei <- definitionOfExpressionVariable nam parSpan
  pure $ ParameterAlone (inf, ei)


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
  case maybeType of
    Just (colonInfo, t) ->
      pure ParameterWithType {name = (inf, ei), colon = colonInfo, _type = t}
    Nothing -> pure $ ParameterAlone (inf, ei)


parametersParserAux
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es [(Parameter, InfoId)]
parametersParserAux = do
  originalState <- getParseState
  start <- parameterParser
  maybeInfoComma <- optional (try comma)
  case maybeInfoComma of
    Nothing -> do
      putParseState originalState
      pure []
    Just infoComma -> do
      remain <- parametersParserAux <|> pure []
      pure ((start, infoComma) : remain)


parametersParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es (Maybe (NonEmpty (Parameter, InfoId)))
parametersParser = do
  result <- parametersParserAux
  pure $ case result of
    (x : y) -> Just (x :| y)
    _ -> Nothing


functionParametersParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff
      es
      (NonEmpty FunctionParameter)
functionParametersParser =
  some
    ( ( (\(a, b, c) -> FunctionParameterWithType a b c)
          <$> between
            leftParen
            rightParen
            parameterParser
      )
        <|> (FunctionParameterAlone <$> parameterAlone)
    )


removeParameters
  :: SymbolResolution :> es
  => NonEmpty Parameter
  -> Eff es ()
removeParameters = mapM_ removeParameter
  where
    removeParameter (ParameterWithType {_type = __type, name = _name}) =
      removeExpressionDefinition (snd _name)
    removeParameter (ParameterAlone {name = _name}) =
      removeExpressionDefinition (snd _name)


functionParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Function
functionParser = do
  startInfo <- lambdaStart
  parameters <- functionParametersParser
  arrowInfo <- rightArrow
  body <- expressionParser
  removeParameters
    ( parameter
        <$> parameters
    )
  pure
    Function'
      { start = startInfo
      , parameters
      , arrow = arrowInfo
      , body
      }


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


definitionParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Definition
definitionParser = do
  (nam, inf, span) <- identifierParser
  ei <- definitionOfExpressionVariable nam span
  maybeColonInfo <- optional Common.colon
  (maybeParams, maybeOutput) <- case maybeColonInfo of
    Nothing -> pure (Nothing, Nothing)
    Just _ -> do
      maybeParams <-
        join <$> (optional parametersParser <?> ('p' :| "arameter"))
      maybeOut <-
        optional typeAtomNoVar
      pure (maybeParams, maybeOut)
  eq <- Common.equal
  definition <- expressionParser
  case maybeParams of
    Nothing -> pure ()
    Just l -> removeParameters (fst <$> l)
  pure
    Definition'
      { name = (inf, ei)
      , colon = maybeColonInfo
      , parameters =
          Parameters' <$> maybeParams
      , outputType = maybeOutput
      , definition
      , equal = eq
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


expressionParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Expression
expressionParser =
  ifParser
    <|> letParser
    <|> (EFunction <$> functionParser)
    <|> applicationParser
