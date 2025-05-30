{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Octizys.Parser.Expression where

import Control.Monad (forM)
import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Effectful (Eff, (:>))
import Octizys.Common.Id (InfoId)
import Octizys.Cst.Expression
  ( Definition
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
  , Function (Function', body, parameters, start)
  , Parameter (ParameterAlone, ParameterWithType, colon, name, _type)
  , Parameters (Parameters', bodySeparator, initParameter, otherParameters)
  , SchemeStart
    ( SchemeStart'
    , dot
    , typeArguments
    , _forall
    )
  )
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
import Octizys.Effects.Parser.Effect (Parser)
import Octizys.Effects.SymbolResolution.Effect
  ( SymbolResolution
  , definitionOfExpressionVariable
  , definitionOfTypeVariable
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
  ((`EBool` True) <$> keyword "True")
    <|> ((`EBool` False) <$> keyword "False")


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
  (boolParser <?> ('b' :| "oolean"))
    <|> (intParser <?> ('i' :| "nteger"))
    <|> (parensExpressionParser <?> 'e' :| "xpression in parenthesis")
    <|> (variableParser <?> 'v' :| "ariable")


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


otherParameterParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es (InfoId, Parameter)
otherParameterParser = do
  commaInfo <- comma
  param <- parameterParser
  pure (commaInfo, param)


parametersParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Parameters
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


removeParameters
  :: SymbolResolution :> es
  => Parameters
  -> Eff es ()
removeParameters Parameters' {initParameter, otherParameters} =
  mapM_
    removeParameter
    (initParameter : (snd <$> otherParameters))
  where
    removeParameter (ParameterWithType {_type = __type, name = _name}) =
      removeExpressionDefinition (snd _name)
    removeParameter (ParameterAlone {name = _name}) =
      removeExpressionDefinition (snd _name)


removeTypeParameters
  :: SymbolResolution :> es
  => SchemeStart
  -> Eff es ()
removeTypeParameters SchemeStart' {typeArguments} =
  mapM_
    removeTypeDefinition
    (snd <$> typeArguments)


functionParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es Function
functionParser = do
  startInfo <- lambdaStart
  parameters <- parametersParser
  body <- expressionParser
  removeParameters parameters
  pure
    Function'
      { start = startInfo
      , parameters
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


schemeStartParser
  :: Parser OctizysParseError :> es
  => SymbolResolution :> es
  => Eff es SchemeStart
schemeStartParser = do
  _forall <- Common.forallKeyword
  paramInfos <- some identifierParser
  arguments <-
    forM
      paramInfos
      ( \(nam, inf, parSpan) -> do
          tvid <- definitionOfTypeVariable nam parSpan
          pure (inf, tvid)
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
  => SymbolResolution :> es
  => Eff es DefinitionTypeAnnotation
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
  => SymbolResolution :> es
  => Eff es Definition
definitionParser = do
  (nam, inf, span) <- identifierParser
  maybeType <- optional definitionTypeAnnotationParser
  eq <- Common.equal
  -- We need to be sure that we are in a definition before
  -- we register the variable, since we say a '=' we know
  -- that this is a definition and can register it.
  ei <- definitionOfExpressionVariable nam span
  definition <- expressionParser
  case maybeType of
    Nothing -> pure ()
    Just
      DefinitionTypeAnnotation'
        { parameters = maybePs
        , schemeStart = maybeTypePs
        } -> do
        maybe (pure ()) removeParameters maybePs
        maybe (pure ()) removeTypeParameters maybeTypePs
  pure
    Definition'
      { name = (inf, ei)
      , _type = maybeType
      , equal = eq
      , definition
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
          def <- definitionParser <?> ('d' :| "efinition")
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
  ( ifParser
      <|> letParser
      <|> (EFunction <$> functionParser)
      <|> applicationParser
  )
    <?> ('e' :| "xpression")
