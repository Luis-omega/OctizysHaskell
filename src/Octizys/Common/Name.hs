module Octizys.Common.Name (Name, makeName, parseName) where

import Control.Arrow ((<<<))
import Data.Aeson (ToJSON, ToJSONKey)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import Effectful (Eff)
import Effectful.Internal.Effect ((:>))
import EffectfulParserCombinators.Combinators (takeWhile1P, takeWhileP)
import EffectfulParserCombinators.Effect (Parser)
import GHC.Generics (Generic, Generically (..))
import GHC.Unicode (isAlpha)
import Octizys.Classes.From (From (from))
import Octizys.FrontEnd.Parser.Error (OctizysParseError)
import Prettyprinter (Pretty (pretty))


{- | The text must be a string without spaces or line breaks.
This is the underlying type used to capture variable names.
-}
newtype Name = Name' {nameRaw :: Text}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Name


instance ToJSONKey Name


instance From Text Name where
  from x = x.nameRaw


isValidNameStartChar :: Char -> Bool
isValidNameStartChar = isAlpha


isValidNameMiddleChar :: Char -> Bool
isValidNameMiddleChar c =
  isAlphaNum c || c == '_'


-- | Smart constructor for Name
makeName :: Text -> Maybe Name
makeName t =
  case Text.uncons t of
    Just (start, remain) ->
      if isValidNameStartChar start
        && Text.all isValidNameMiddleChar remain
        then Just (Name' t)
        else Nothing
    Nothing -> Nothing


-- It is here to avoid exposing the Name constructor
-- or the use of a "unsafe" function.
parseName
  :: Parser OctizysParseError :> es
  => Eff es Name
parseName = do
  _head <-
    takeWhile1P (Just "identifier start character") isValidNameStartChar
  remain <-
    takeWhileP
      isValidNameMiddleChar
  let full_string = _head <> remain
  pure $ Name' full_string


instance Pretty Name where
  pretty = pretty <<< nameRaw
