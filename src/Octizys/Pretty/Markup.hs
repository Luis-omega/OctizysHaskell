module Octizys.Pretty.Markup where

import Data.Set


data Color = White | Black | Blue | Cian | Green | Red | Yellow
  deriving (Show, Eq, Ord)


data Style = Bold | Italic | Underline
  deriving (Show, Eq, Ord)


data Markup = Markup'
  { color :: Color
  , style :: Set Style
  }
  deriving (Show, Eq, Ord)

emptyStyle :: Set Style
emptyStyle = mempty


setColor :: Color -> Markup -> Markup
setColor c m = m{color = c}


setStyle :: Set Style -> Markup -> Markup
setStyle s m = m{style = s}

addStyle :: Style -> Markup -> Markup
addStyle s m = m{style = insert s m.style }

removeStyle :: Style -> Markup -> Markup
removeStyle s m = m{style = delete s m.style }

cleanStyle :: Markup -> Markup
cleanStyle = setStyle mempty
