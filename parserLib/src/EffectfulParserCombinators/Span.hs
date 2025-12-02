module EffectfulParserCombinators.Span
  ( Position
      ( Position'
      , line
      , column
      , offset
      )
  , makeInitialPosition
  , Span (Span', start, end)
  ) where


data Position = Position'
  { line :: Int
  -- ^ The amount of "\n" seen on stream
  , column :: Int
  -- ^ The amount of `Text` `Char`.
  , offset :: Int
  -- ^ The position on `Text` `Char` (ie unicode)
  }
  -- TODO: Use bytes

  deriving (Show, Eq, Ord)


makeInitialPosition :: Position
makeInitialPosition = Position' {line = 0, column = 0, offset = 0}


{- | Represents the start and end positions
of an input.
-}
data Span = Span'
  { start :: Position
  , end :: Position
  }
  deriving (Show, Eq, Ord)
