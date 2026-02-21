{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Octizys.FrontEnd.Parser.Expression where

import Control.Arrow ((<<<))
import Control.Monad (forM)
import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Effectful (Eff, (:>))
import EffectfulParserCombinators.Combinators
  ( many
  , optional
  , some
  , takeWhile1P
  , takeWhileP
  , try
  , (<?>)
  , (<|>)
  )
import EffectfulParserCombinators.Effect (Parser)
import Octizys.Classes.From (From (from))
import Octizys.FrontEnd.Cst.Expression
  ( Annotation (Annotation', colon, expression, _type)
  , Application (Application', function, remain)
  , BoolExpression (BoolExpression')
  , Definition
    ( Definition'
    , definition
    , equal
    , name
    , _type
    )
  , DefinitionTypeAnnotation
    ( DefinitionTypeAnnotation'
    , colon
    , outputType
    , parameters
    , schemeStart
    )
  , Expression
  , Function (Function', body, parameters, start)
  , If (If', condition, ifFalse, ifTrue, _else, _if, _then)
  , IntExpression (IntExpression', info, value)
  , Let (Let', definitions, expression, _in, _let)
  , Parameter (ParameterAlone, ParameterWithType, colon, name, _type)
  , Parameters (..)
  , Parens (Parens', expression, lparen, rparen)
  , SchemeStart (SchemeStart', dot, typeArguments, _forall)
  , Variable (Variable', info, name)
  )
import Octizys.FrontEnd.Cst.SourceInfo (SourceInfo, SourceVariable)
import Octizys.FrontEnd.Cst.Type (Type)
import Octizys.FrontEnd.Parser.Common
  ( between
  , comma
  , elseKeyword
  , ifKeyword
  , inKeyword
  , keyword
  , lambdaStart
  , leftParen
  , letKeyword
  , localVariable
  , rightParen
  , semicolon
  , sourceVariableParser
  , thenKeyword
  , tokenAndregister
  )
import qualified Octizys.FrontEnd.Parser.Common as Common
import Octizys.FrontEnd.Parser.Error
  ( OctizysParseError
  )
import Octizys.FrontEnd.Parser.Type (parseType)
import Prelude hiding (span)


parseExpression
  :: Parser OctizysParseError
    :> es
  => Eff es (Expression SourceVariable SourceVariable)
parseExpression = expressionParser


-- ======================= Literals ===========================

boolParser
  :: Parser OctizysParseError :> es
  => Eff es (Expression SourceVariable SourceVariable)
boolParser =
  ((from <<< (`BoolExpression'` True)) <$> keyword "True")
    <|> ((from <<< (`BoolExpression'` False)) <$> keyword "False")


intParser
  :: Parser OctizysParseError :> es
  => Eff es (Expression SourceVariable SourceVariable)
intParser = do
  (value, inf) <-
    tokenAndregister
      ( do
          _head <- takeWhile1P (Just "digit") isDigit
          others <- takeWhileP (\c -> isDigit c || c == '_')
          pure (_head <> others)
      )
      <?> ('v' :| "alid integer")
  pure $ from $ IntExpression' {info = inf, value = value}


-- ======================= Expression ===========================

typeAnnotationParser
  :: Parser OctizysParseError :> es
  => Eff es (SourceInfo, Type SourceVariable)
typeAnnotationParser = do
  colonInfo <- Common.colon
  _type <- parseType
  pure (colonInfo, _type)


variableParser
  :: Parser OctizysParseError :> es
  => Eff es (Expression SourceVariable SourceVariable)
variableParser = do
  (name, info) <- sourceVariableParser
  pure $ from $ Variable' {info, name}


maybeAnnotation
  :: Parser OctizysParseError :> es
  => Eff es (Expression SourceVariable SourceVariable)
maybeAnnotation = do
  expr <- parseExpression
  maybeType <- optional typeAnnotationParser
  case maybeType of
    Just (colonInfo, _type) ->
      pure $ from $ Annotation' {expression = expr, colon = colonInfo, _type}
    Nothing -> pure expr


parensExpressionParser
  :: Parser OctizysParseError :> es
  => Eff es (Expression SourceVariable SourceVariable)
parensExpressionParser = do
  (lparen, expression, rparen) <-
    between
      leftParen
      rightParen
      maybeAnnotation
  pure $ from $ Parens' {lparen, rparen, expression}


atomExpressionParser
  :: Parser OctizysParseError :> es
  => Eff es (Expression SourceVariable SourceVariable)
atomExpressionParser =
  (boolParser <?> ('b' :| "oolean"))
    <|> (intParser <?> ('i' :| "nteger"))
    <|> (parensExpressionParser <?> 'e' :| "xpression in parenthesis")
    <|> (variableParser <?> 'v' :| "ariable")


applicationParser
  :: Parser OctizysParseError :> es
  => Eff es (Expression SourceVariable SourceVariable)
applicationParser = do
  function <- atomExpressionParser
  -- the Try is a fix, if the parser of a identifier
  -- fails, then this will fail and we don't want that.
  arguments <- many (try atomExpressionParser)
  case arguments of
    [] -> pure function
    (ini : las) ->
      pure $
        from $
          Application'
            { function = function
            , remain = ini :| las
            }


parameterParser
  :: Parser OctizysParseError :> es
  => Eff es (Parameter SourceVariable SourceVariable)
parameterParser = do
  -- TODO : add support to expression holes
  (var, varInfo) <- localVariable
  maybeType <-
    optional
      ( do
          colonInfo <- Common.colon
          _type <- parseType
          pure (colonInfo, _type)
      )
  case maybeType of
    Just (colonInfo, t) ->
      pure
        ParameterWithType
          { name = (varInfo, var)
          , colon = colonInfo
          , _type = t
          }
    Nothing -> pure $ ParameterAlone (varInfo, var)


otherParameterParser
  :: Parser OctizysParseError :> es
  => Eff
      es
      ( SourceInfo
      , Parameter SourceVariable SourceVariable
      )
otherParameterParser = do
  commaInfo <- comma
  param <- parameterParser
  pure (commaInfo, param)


parametersParser
  :: Parser OctizysParseError :> es
  => Eff es (Parameters SourceVariable SourceVariable)
parametersParser = do
  initParameter <- parameterParser
  otherParameters <- many otherParameterParser
  bodySeparator <- Common.turnstile
  pure
    Parameters'
      { initParameter
      , otherParameters
      , bodySeparator
      }


functionParser
  :: Parser OctizysParseError :> es
  => Eff es (Expression SourceVariable SourceVariable)
functionParser = do
  startInfo <- lambdaStart
  parameters <- parametersParser
  body <- expressionParser
  pure $
    from
      Function'
        { start = startInfo
        , parameters
        , body
        }


ifParser
  :: Parser OctizysParseError :> es
  => Eff es (Expression SourceVariable SourceVariable)
ifParser = do
  _if <- ifKeyword
  condition <- expressionParser
  _then <- thenKeyword
  ifTrue <- expressionParser
  _else <- elseKeyword
  ifFalse <- expressionParser
  pure $ from $ If' {..}


schemeStartParser
  :: Parser OctizysParseError :> es
  => Eff es (SchemeStart SourceVariable)
schemeStartParser = do
  _forall <- Common.forallKeyword
  paramInfos <- some (localVariable <?> ('t' :| "ype variable"))
  arguments <-
    forM
      paramInfos
      ( \(var, varInfo) -> do
          pure (varInfo, var)
      )
  _dot <- Common.dot
  pure
    SchemeStart'
      { _forall
      , typeArguments = arguments
      , dot = _dot
      }


definitionTypeAnnotationParser
  :: Parser OctizysParseError :> es
  => Eff es (DefinitionTypeAnnotation SourceVariable SourceVariable)
definitionTypeAnnotationParser = do
  colon <- Common.colon
  schemeStart <- optional schemeStartParser
  parameters <- optional parametersParser
  outputType <- parseType
  pure
    DefinitionTypeAnnotation'
      { colon
      , schemeStart
      , parameters
      , outputType
      }


definitionParser
  :: Parser OctizysParseError :> es
  => Eff es (Definition SourceVariable SourceVariable)
definitionParser = do
  (definitionName, info) <- localVariable <?> ('i' :| "dentifier")
  maybeType <- optional definitionTypeAnnotationParser
  eq <- Common.equal
  definition <- expressionParser
  pure
    Definition'
      { name = (info, definitionName)
      , _type = maybeType
      , equal = eq
      , definition
      }


letParser
  :: Parser OctizysParseError :> es
  => Eff es (Expression SourceVariable SourceVariable)
letParser = do
  _let <- letKeyword
  definitions <-
    some
      ( do
          def <- definitionParser <?> ('d' :| "efinition")
          semiInfo <- semicolon
          pure (def, semiInfo)
      )
  _in <- inKeyword
  expression <- parseExpression
  pure $ from $ Let' {_let, definitions, _in, expression}


expressionParser
  :: Parser OctizysParseError :> es
  => Eff es (Expression SourceVariable SourceVariable)
expressionParser =
  ( ifParser
      <|> letParser
      <|> functionParser
      <|> applicationParser
  )
    <?> ('e' :| "xpression")
