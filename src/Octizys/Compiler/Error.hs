module Octizys.Compiler.Error (Error (ParserError, FileReadError)) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import qualified EffectfulParserCombinators.Error as Parser
import GHC.Generics (Generic, Generically (..))
import Octizys.Classes.From (From (from))
import Octizys.Common.LogicPath (LogicPath)
import qualified Octizys.Effects.FileReader.Effect as FileReader
import Octizys.FrontEnd.Parser.Error (OctizysParseError)
import Prettyprinter (Pretty (pretty))


data Error
  = ParserError
      FilePath
      LogicPath
      Text
      (Parser.ParserError OctizysParseError)
  | FileReadError FileReader.FileReadError
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via Generically Error


instance From Error FileReader.FileReadError where
  from = FileReadError


-- TODO:STUB
instance Pretty Error where
  pretty (ParserError _ _ src parseError) =
    Parser.humanReadableError Nothing src parseError
  pretty (FileReadError e) = pretty e
