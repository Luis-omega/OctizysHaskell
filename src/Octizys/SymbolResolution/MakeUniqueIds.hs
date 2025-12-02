module Octizys.SymbolResolution.MakeUniqueIds
  ( makeVariablesUnique
  , makeDefinitionsUnique
  , Context
    ( Context'
    , evarNames
    , evarMap
    , tvarMap
    , tvarNames
    )
  , SymbolResolutionState
    ( SymbolResolutionState'
    , context
    , warnings
    , errors
    )
  ) where

import Control.Monad (forM, forM_)
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, get, modify)
import Octizys.Common.HistoryMap (HistoryMap)
import qualified Octizys.Common.HistoryMap as HistoryMap
import Octizys.Common.Id
  ( ExpressionVariableId
  , TypeVariableId
  )
import Octizys.Effects.IdGenerator.Interpreter (IdGenerator, generateId)
import Octizys.FrontEnd.Cst.Expression
  ( Definition
      ( Definition'
      , definition
      , equal
      , name
      , _type
      )
  , DefinitionTypeAnnotation
    ( DefinitionTypeAnnotation'
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
    , Variable
    , applicationFunction
    , applicationRemain
    , body
    , condition
    , definitions
    , expression
    , ifFalse
    , ifTrue
    , info
    , lparen
    , name
    , parameters
    , rparen
    , start
    , _type
    )
  , Parameter
    ( ParameterAlone
    , ParameterWithType
    , name
    , _type
    )
  , Parameters
    ( Parameters'
    , bodySeparator
    , initParameter
    , otherParameters
    )
  , SchemeStart (typeArguments)
  )
import qualified Octizys.FrontEnd.Cst.Expression as E
import Octizys.FrontEnd.Cst.SourceInfo (SourceInfo, SourceVariable)
import Octizys.FrontEnd.Cst.Type
  ( Type
      ( Arrow
      , BoolType
      , IntType
      , Parens
      , TVariable
      , info
      , lparen
      , remain
      , rparen
      , start
      , variable
      , _type
      )
  )


data Context = Context'
  { evarNames :: HistoryMap SourceVariable ExpressionVariableId
  , tvarNames :: HistoryMap SourceVariable TypeVariableId
  , evarMap :: Map ExpressionVariableId SourceVariable
  , tvarMap :: Map TypeVariableId SourceVariable
  }


data SymbolResolutionWarning
  = EVarShadowing SourceVariable ExpressionVariableId
  | TVarShadowing SourceVariable TypeVariableId
  deriving (Show, Eq, Ord)


data SymbolResolutionError
  = UnboundTypeVariable SourceVariable SourceInfo
  | UnboundExpVariable SourceVariable SourceInfo
  deriving (Show, Eq, Ord)


data SymbolResolutionState = SymbolResolutionState'
  { context :: Context
  , warnings :: [SymbolResolutionWarning]
  , errors :: [SymbolResolutionError]
  }


{- | Lookups if a variable with the given
name is know to the system.
-}
lookupEvar
  :: State SymbolResolutionState :> es
  => SourceVariable
  -> Eff es (Maybe ExpressionVariableId)
lookupEvar var = do
  s <- get
  let result = HistoryMap.lookup var s.context.evarNames
  pure result


{- | To use at the place of binding of a expression variable.
Right now the binding place can be:

- A Module global definition.
- A Left side of a let definition.
For a variable inside the body of another expression,
call `findEvar`
-}
registerNewEvar
  :: State SymbolResolutionState :> es
  => IdGenerator ExpressionVariableId :> es
  => SourceVariable
  -> Eff es ExpressionVariableId
registerNewEvar var = do
  maybeKnow <- lookupEvar var
  case maybeKnow of
    Nothing -> pure ()
    Just knowId ->
      tellWarn (EVarShadowing var knowId)
  varId <- generateId @ExpressionVariableId
  modify
    ( \s ->
        let
          newEvarNames =
            HistoryMap.pushValue
              (var, varId)
              s.context.evarNames
          newEvarMap = Map.insert varId var s.context.evarMap
         in
          s
            { context =
                s.context
                  { evarNames =
                      newEvarNames
                  , evarMap = newEvarMap
                  }
            }
    )
  pure varId


deregisterEvar
  :: State SymbolResolutionState :> es
  => ExpressionVariableId
  -> SourceVariable
  -> Eff es ()
deregisterEvar eid var =
  modify
    ( \s ->
        let newEvarMap = Map.delete eid s.context.evarMap
            newEvarNames =
              HistoryMap.popValue
                var
                s.context.evarNames
         in s
              { context =
                  s.context
                    { evarNames =
                        newEvarNames
                    , evarMap = newEvarMap
                    }
              }
    )


{- | Attempts to find a expression variable in the environment,
if it isn't found it generates a new identifier for it,
but it won't register it. Additionally we create a new
error for this unbound variable.
This means that we thread every occurrence of a unbound
variable as a occurrence of a different variable.
-}
findEvar
  :: State SymbolResolutionState :> es
  => IdGenerator ExpressionVariableId :> es
  => SourceVariable
  -> SourceInfo
  -> Eff es ExpressionVariableId
findEvar var inf = do
  maybeKnow <- lookupEvar var
  case maybeKnow of
    Nothing -> do
      tellError (UnboundExpVariable var inf)
      varId <- generateId @ExpressionVariableId
      modify
        ( \s ->
            let
              newEvarMap = Map.insert varId var s.context.evarMap
             in
              s
                { context =
                    s.context
                      { evarMap = newEvarMap
                      }
                }
        )
      pure varId
    Just knowId ->
      pure knowId


lookupTvar
  :: State SymbolResolutionState :> es
  => SourceVariable
  -> Eff es (Maybe TypeVariableId)
lookupTvar var = do
  s <- get
  let result = HistoryMap.lookup var s.context.tvarNames
  pure result


registerNewTvar
  :: State SymbolResolutionState :> es
  => IdGenerator TypeVariableId :> es
  => SourceVariable
  -> Eff es TypeVariableId
registerNewTvar var = do
  maybeKnow <- lookupTvar var
  case maybeKnow of
    Nothing -> pure ()
    Just knowId ->
      tellWarn (TVarShadowing var knowId)
  varId <- generateId @TypeVariableId
  modify
    ( \s ->
        let
          newTvarNames =
            HistoryMap.pushValue
              (var, varId)
              s.context.tvarNames
          newTvarMap = Map.insert varId var s.context.tvarMap
         in
          s
            { context =
                s.context
                  { tvarNames =
                      newTvarNames
                  , tvarMap = newTvarMap
                  }
            }
    )
  pure varId


deregisterTvar
  :: State SymbolResolutionState :> es
  => TypeVariableId
  -> SourceVariable
  -> Eff es ()
deregisterTvar tid var =
  modify
    ( \s ->
        let newTvarMap = Map.delete tid s.context.tvarMap
            newTvarNames =
              HistoryMap.popValue
                var
                s.context.tvarNames
         in s
              { context =
                  s.context
                    { tvarNames =
                        newTvarNames
                    , tvarMap = newTvarMap
                    }
              }
    )


{- | Attempts to find a type variable in the environment,
if it isn't found it generates a new identifier for it,
but it won't register it. Additionally we create a new
error for this unbound variable.
This means that we thread every occurrence of a unbound
variable as a occurrence of a different variable.
-}
findTvar
  :: State SymbolResolutionState :> es
  => IdGenerator TypeVariableId :> es
  => SourceVariable
  -> SourceInfo
  -> Eff es TypeVariableId
findTvar var inf = do
  maybeKnow <- lookupTvar var
  case maybeKnow of
    Nothing -> do
      tellError (UnboundTypeVariable var inf)
      varId <- generateId @TypeVariableId
      modify
        ( \s ->
            let
              newTvarMap = Map.insert varId var s.context.tvarMap
             in
              s
                { context =
                    s.context
                      { tvarMap = newTvarMap
                      }
                }
        )
      pure varId
    Just knowId ->
      pure knowId


tellWarn
  :: State SymbolResolutionState :> es
  => SymbolResolutionWarning
  -> Eff es ()
tellWarn w = modify (\s -> s {warnings = w : s.warnings})


tellError
  :: State SymbolResolutionState :> es
  => SymbolResolutionError
  -> Eff es ()
tellError w = modify (\s -> s {errors = w : s.errors})


{- | To use to translate a type with source variables
to a type with unique ids.
This shouldn't handle scopes, only lookup for type variables.
Is responsibility of `makeVariablesUnique` to add them
to the scope at scheme places or if is a global type, to
be added to global scope before calling this function.
-}
makeTypeVariablesUnique
  :: State SymbolResolutionState :> es
  => IdGenerator TypeVariableId :> es
  => Type SourceVariable
  -> Eff es (Type TypeVariableId)
makeTypeVariablesUnique t =
  case t of
    BoolType {info} -> pure BoolType {info = info}
    IntType {info} -> pure IntType {info = info}
    Arrow {start, remain} -> do
      startNew <- makeTypeVariablesUnique start
      remainNew <-
        forM
          remain
          ( \(x, y) ->
              do
                result <- makeTypeVariablesUnique y
                pure (x, result)
          )
      pure Arrow {start = startNew, remain = remainNew}
    Parens {lparen, rparen, _type} -> do
      newType <- makeTypeVariablesUnique _type
      pure Parens {lparen, rparen, _type = newType}
    TVariable {info, variable} -> do
      varId <- findTvar variable info
      pure
        TVariable
          { info
          , variable = varId
          }


makeParameterUnique
  :: ( State SymbolResolutionState :> es
     , IdGenerator ExpressionVariableId :> es
     , IdGenerator TypeVariableId :> es
     )
  => Parameter SourceVariable SourceVariable
  -> Eff es (Parameter ExpressionVariableId TypeVariableId)
makeParameterUnique p = do
  parameterId <- registerNewEvar (snd p.name)
  case p of
    ParameterAlone {name = (src, _)} ->
      pure ParameterAlone {name = (src, parameterId)}
    ParameterWithType {name = (src, _), colon, _type} -> do
      newType <- makeTypeVariablesUnique _type
      pure
        ParameterWithType
          { name = (src, parameterId)
          , colon = colon
          , _type = newType
          }


deregisterParameter
  :: State SymbolResolutionState :> es
  => Parameter ExpressionVariableId TypeVariableId
  -> Parameter SourceVariable SourceVariable
  -> Eff es ()
deregisterParameter pid pvar =
  deregisterEvar (snd pid.name) (snd pvar.name)


makeParametersUnique
  :: ( State SymbolResolutionState :> es
     , IdGenerator ExpressionVariableId :> es
     , IdGenerator TypeVariableId :> es
     )
  => Parameters SourceVariable SourceVariable
  -> Eff es (Parameters ExpressionVariableId TypeVariableId)
makeParametersUnique
  Parameters'
    { initParameter
    , otherParameters
    , bodySeparator
    } = do
    newInit <- makeParameterUnique initParameter
    newOthers <-
      forM
        otherParameters
        ( \(x, y) -> do
            new <- makeParameterUnique y
            pure (x, new)
        )
    pure
      Parameters'
        { initParameter = newInit
        , otherParameters = newOthers
        , bodySeparator
        }


deregisterParameters
  :: State SymbolResolutionState :> es
  => Parameters ExpressionVariableId TypeVariableId
  -> Parameters SourceVariable SourceVariable
  -> Eff es ()
deregisterParameters pids pvars =
  forM_
    ( zip
        (pids.initParameter : (snd <$> pids.otherParameters))
        (pvars.initParameter : (snd <$> pvars.otherParameters))
    )
    dereg
  where
    dereg (x, y) = deregisterParameter x y


makeSchemeStartUnique
  :: ( State SymbolResolutionState :> es
     , IdGenerator TypeVariableId :> es
     )
  => SchemeStart SourceVariable
  -> Eff es (SchemeStart TypeVariableId)
makeSchemeStartUnique s = do
  newArguments <- forM s.typeArguments processArgument
  pure s {typeArguments = newArguments}
  where
    processArgument (src, arg) = do
      newId <- registerNewTvar arg
      pure (src, newId)


deregisterScheme
  :: State SymbolResolutionState :> es
  => SchemeStart TypeVariableId
  -> SchemeStart SourceVariable
  -> Eff es ()
deregisterScheme sid svar =
  let
    zipped =
      zip
        (NonEmpty.toList (snd <$> sid.typeArguments))
        (NonEmpty.toList (snd <$> svar.typeArguments))
   in
    forM_ zipped (uncurry deregisterTvar)


makeDefinitionTypeAnnotationUnique
  :: ( State SymbolResolutionState :> es
     , IdGenerator TypeVariableId :> es
     , IdGenerator ExpressionVariableId :> es
     )
  => DefinitionTypeAnnotation SourceVariable SourceVariable
  -> Eff
      es
      ( DefinitionTypeAnnotation
          ExpressionVariableId
          TypeVariableId
      )
makeDefinitionTypeAnnotationUnique d = do
  newScheme <- traverse makeSchemeStartUnique d.schemeStart
  newParams <- traverse makeParametersUnique d.parameters
  newType <- makeTypeVariablesUnique d.outputType
  pure
    DefinitionTypeAnnotation'
      { colon = d.colon
      , schemeStart = newScheme
      , parameters = newParams
      , outputType = newType
      }


deregisterDefinitionTypeAnnotation
  :: State SymbolResolutionState :> es
  => DefinitionTypeAnnotation ExpressionVariableId TypeVariableId
  -> DefinitionTypeAnnotation SourceVariable SourceVariable
  -> Eff es ()
deregisterDefinitionTypeAnnotation did dvar =
  case did.schemeStart of
    Just sNew ->
      traverse_
        (deregisterScheme sNew)
        dvar.schemeStart
    _ -> pure ()


makeDefinitionUnique
  :: ( State SymbolResolutionState :> es
     , IdGenerator TypeVariableId :> es
     , IdGenerator ExpressionVariableId :> es
     )
  => Definition SourceVariable SourceVariable
  -> Eff
      es
      ( Definition
          ExpressionVariableId
          TypeVariableId
      )
makeDefinitionUnique
  Definition'
    { name
    , _type
    , equal
    , definition
    } = do
    -- Thanks to recursive definitions, we define the variable
    -- before calling this function.
    newName <- findEvar (snd name) (fst name)
    newType <- traverse makeDefinitionTypeAnnotationUnique _type
    newDef <- makeVariablesUnique definition
    -- The scope of type schemes doesn't extend to other
    -- functions inside a recursive definition.
    case newType of
      Just tNew ->
        traverse_ (deregisterDefinitionTypeAnnotation tNew) _type
      _ -> pure ()
    pure
      Definition'
        { name = (fst name, newName)
        , _type = newType
        , equal = equal
        , definition = newDef
        }


makeDefinitionsUnique
  :: State SymbolResolutionState :> es
  => IdGenerator ExpressionVariableId :> es
  => IdGenerator TypeVariableId :> es
  => Traversable t
  => t (Definition SourceVariable SourceVariable, SourceInfo)
  -> Eff
      es
      ( t (Definition ExpressionVariableId TypeVariableId, SourceInfo)
      , t (ExpressionVariableId, SourceVariable)
      )
makeDefinitionsUnique definitions = do
  names <-
    forM
      definitions
      ( \(d, _) -> do
          new <- registerNewEvar (snd d.name)
          pure (new, snd d.name)
      )
  newDefs <-
    forM
      definitions
      ( \(x, y) -> do
          new <- makeDefinitionUnique x
          pure (new, y)
      )
  pure (newDefs, names)


makeVariablesUnique
  :: State SymbolResolutionState :> es
  => IdGenerator ExpressionVariableId :> es
  => IdGenerator TypeVariableId :> es
  => Expression SourceVariable SourceVariable
  -> Eff es (Expression ExpressionVariableId TypeVariableId)
makeVariablesUnique e =
  case e of
    EInt {..} -> pure EInt {..}
    EBool {..} -> pure EBool {..}
    Variable {info, name} -> do
      varId <- findEvar name info
      pure Variable {info, name = varId}
    E.Parens {lparen, rparen, expression} -> do
      newExp <- makeVariablesUnique expression
      pure e {expression = newExp, lparen, rparen}
    EFunction
      { start
      , parameters
      , body
      } ->
        do
          newParams <- makeParametersUnique parameters
          newBody <- makeVariablesUnique body
          deregisterParameters newParams parameters
          pure
            EFunction
              { start
              , parameters = newParams
              , body = newBody
              }
    Application {applicationFunction, applicationRemain} -> do
      newFunc <- makeVariablesUnique applicationFunction
      newRemain <- forM applicationRemain makeVariablesUnique
      pure
        Application
          { applicationFunction = newFunc
          , applicationRemain = newRemain
          }
    If {condition, ifTrue, ifFalse} -> do
      newCond <- makeVariablesUnique condition
      newTrue <- makeVariablesUnique ifTrue
      newFalse <- makeVariablesUnique ifFalse
      pure
        e
          { condition = newCond
          , ifTrue = newTrue
          , ifFalse = newFalse
          }
    Let {definitions, expression} -> do
      (newDefs, names) <- makeDefinitionsUnique definitions
      newExp <- makeVariablesUnique expression
      forM_ names (uncurry deregisterEvar)
      pure
        e
          { definitions = newDefs
          , expression = newExp
          }
    Annotation
      { expression
      , _type
      } -> do
        newExp <- makeVariablesUnique expression
        newType <- makeTypeVariablesUnique _type
        pure
          e
            { expression = newExp
            , _type = newType
            }
