{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Octizys.Inference.Errors where

import qualified Octizys.Cst.Expression as CstE

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Octizys.Ast.Node as Ast
import Octizys.Ast.Type (InferenceVariable)
import qualified Octizys.Ast.Type as AstT
import Octizys.Cst.Expression (ExpressionVariableId)
import qualified Octizys.Cst.Node as Cst
import Octizys.Cst.Type
  ( TypeVariableId
  )
import Octizys.Pretty.FormatContext
  ( FormatContext
  , formatTypeVar
  , nest
  , setShowTypeVar
  , shouldShowConstraintReason
  )
import Octizys.Pretty.Formatter (Formatter (format))
import Octizys.Report
  ( LongDescription (LongDescription', afterDescription, preDescription, source)
  )
import Prettyprinter (Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty
import Prelude hiding (lookup)


data InferenceError
  = -- This shouldn't happen, is a bug.
    FunctionWithoutParams CstE.Expression
  | -- This is a bug in the translation process
    UnboundExpressionVar ExpressionVariableId
  | CantUnify
      (AstT.Type InferenceVariable)
      (AstT.Type InferenceVariable)
  | ContainsFreeVariablesAfterSolving
      Cst.Node
      (Ast.Node InferenceVariable)
      (Set.Set TypeVariableId)
      Substitution
  | RecursiveSubstitution Substitution
  deriving (Show)


data ConstraintReason
  = -- | The condition inside an if should be a boolean type
    IfConditionShouldBeBool
  | -- | The cases inside a if should have the same type.
    IfCasesShouldMatch
  | ApplicationShouldBeOnArrows
  | ArgumentShouldBeOfDomainType
  | TypeAnnotation
  | -- | A parameter was found with type annotation
    ParameterTypeAnnotation
  | -- | A variable was defined without arguments, we must be sure
    -- that it's body has the same type as the one assigned to the name.
    -- `let f = body` => `type f ~ type body`
    DefinitionVariableAndBodyShouldBeEqual
  | -- | f : Type
    DefinitionTypeAnnotation
  | -- | f : x , y , Type
    DefinitionTypeAnnotationWithArgs
  | -- | We already know the type of a variable, it can be
    -- because is a builtin or we are in the repl and we solved
    -- it previously.
    IsKnowType
  deriving (Show, Eq, Ord)


instance Formatter ann (FormatContext ann) ConstraintReason where
  format _ IfConditionShouldBeBool = "If condition must be of type bool."
  format _ IfCasesShouldMatch = "If branch cases must have same type."
  format _ ApplicationShouldBeOnArrows = "Application must be only done by functions."
  format _ ArgumentShouldBeOfDomainType = "Argument of a function should have it's domain type."
  format _ TypeAnnotation = "Type annotation."
  format _ ParameterTypeAnnotation = "Type annotation on parameter."
  format _ DefinitionVariableAndBodyShouldBeEqual = "Inferred type of variable and it's definition must match."
  format _ DefinitionTypeAnnotation = "Type annotation on variable definition."
  format _ DefinitionTypeAnnotationWithArgs = "Type annotation on function definition."
  format _ IsKnowType = "Type was already inferred previously or is an import or built in."


data ConstraintInfo = ConstraintInfo'
  { reason :: ConstraintReason
  , cst :: Cst.Node
  -- ^ The original node that instigated
  -- the constraint.
  , ast :: Ast.Node InferenceVariable
  -- ^ The node translated to ast, it contains
  -- all the info to report errors!
  }
  deriving (Show, Ord, Eq)


data Constraint = Constraint'
  { constraintType1 :: AstT.Type InferenceVariable
  , constraintType2 :: AstT.Type InferenceVariable
  , constraintInfo :: ConstraintInfo
  }
  deriving (Show, Ord, Eq)


getConstraintCst :: Constraint -> Cst.Node
getConstraintCst c = c.constraintInfo.cst


getConstraintAst :: Constraint -> Ast.Node InferenceVariable
getConstraintAst c = c.constraintInfo.ast


getConstraintReason :: Constraint -> ConstraintReason
getConstraintReason c = c.constraintInfo.reason


instance Formatter ann (FormatContext ann) Constraint where
  format ctx c =
    let basicDoc =
          format ctx c.constraintType1 <+> "~" <+> format ctx c.constraintType2
     in if shouldShowConstraintReason ctx
          then basicDoc <+> format ctx c.constraintInfo.reason
          else basicDoc


newtype Substitution = Substitution'
  { substitutionMap
      :: Map TypeVariableId (Either InferenceError (AstT.Type InferenceVariable))
  }
  deriving (Show)
  deriving
    (Monoid, Semigroup)
    via ( Map
            TypeVariableId
            ( Either
                InferenceError
                (AstT.Type InferenceVariable)
            )
        )


instance Formatter ann (FormatContext ann) Substitution where
  format ctx s =
    Pretty.list
      ( ( \(ty, v) ->
            Pretty.parens
              ( formatTypeVar ctx ty
                  <> pretty ','
                  <> case v of
                    Left e -> format ctx (Text.pack $ show e)
                    Right y -> format ctx y
              )
        )
          <$> Map.toList s.substitutionMap
      )


singletonSubstitution
  :: TypeVariableId
  -> AstT.Type InferenceVariable
  -> Substitution
singletonSubstitution x v =
  Substitution' (Map.singleton x (Right v))


buildConstraintUnifyReportDescriptions
  :: FormatContext ann
  -> Constraint
  -> (Text, [LongDescription ann])
buildConstraintUnifyReportDescriptions ctx c =
  let
    locationAstDoc = format (setShowTypeVar True ctx) (getConstraintAst c)
    start =
      LongDescription'
        { preDescription = Just "Attempted to unify:"
        , source =
            Just
              ( pretty @Text "  "
                  <> format ctx c.constraintType1
                  <> Pretty.line
                  <> pretty @Text "with:"
                  <> nest
                    ctx
                    ( Pretty.line <> format ctx c.constraintType2
                    )
              )
        , afterDescription = Nothing
        }

    long =
      case getConstraintReason c of
        IfConditionShouldBeBool ->
          [ start
          , LongDescription'
              { preDescription = Just "While trying to check the type of an 'if' condition:"
              , source = Just locationAstDoc
              , afterDescription =
                  Just "A condition in a if expression must be of type Bool."
              }
          ]
        IfCasesShouldMatch ->
          [ start
          , LongDescription'
              { preDescription =
                  Just
                    "While trying to check that both branches of an 'if' expression have the same type:"
              , source = Just locationAstDoc
              , afterDescription =
                  Just
                    "The 'then' and 'else' branches of an 'if' expression must have the same type, which becomes the type of the entire expression."
              }
          ]
        -- TODO: we can try to  get the offending left side!
        ApplicationShouldBeOnArrows ->
          [ start
          , LongDescription'
              { preDescription =
                  Just
                    "While trying to check a function application:"
              , source = Just locationAstDoc
              , afterDescription =
                  Just
                    "The expression being applied must have a function type."
              }
          ]
        -- TODO: we can try to get the right side!
        ArgumentShouldBeOfDomainType ->
          [ start
          , LongDescription'
              { preDescription =
                  Just
                    "While trying to check a function argument:"
              , source = Just locationAstDoc
              , afterDescription =
                  Just
                    "To apply a function to an expression the argument must have the correct type."
              }
          ]
        TypeAnnotation ->
          [ start
          , LongDescription'
              { preDescription =
                  Just
                    "While trying to check that the expression has the given type:"
              , source = Just locationAstDoc
              , afterDescription = Nothing
              }
          ]
        ParameterTypeAnnotation ->
          [ start
          , LongDescription'
              { preDescription =
                  Just
                    "While trying to check the type of the parameter:"
              , source = Just locationAstDoc
              , afterDescription =
                  Just
                    "A type annotation was provided for the parameter, but it conflicts with the type inferred from the function body."
              }
          ]
        DefinitionVariableAndBodyShouldBeEqual ->
          [ start
          , LongDescription'
              { preDescription =
                  Just
                    "While trying to check that the variable and the value have the same type:"
              , source = Just locationAstDoc
              , afterDescription =
                  Just
                    "A definition of a variable without arguments must have the same type as the value assigned to it."
              }
          ]
        DefinitionTypeAnnotation ->
          [ start
          , LongDescription'
              { preDescription =
                  Just
                    "While trying to check that the variable and the body have the same type:"
              , source = Just locationAstDoc
              , afterDescription =
                  Just
                    "The given value has a different type than the one specified in the variable's type signature."
              }
          ]
        DefinitionTypeAnnotationWithArgs ->
          [ start
          , LongDescription'
              { preDescription =
                  Just
                    "While trying to check that the variable and the body have the same type:"
              , source = Just locationAstDoc
              , afterDescription =
                  Just
                    "The given value has a different type than the one specified in the variable's type signature."
              }
          ]
        IsKnowType ->
          -- TODO: what can I put here?
          [start]
   in
    ( "TypeError>Can't unify types."
    , long
    )
