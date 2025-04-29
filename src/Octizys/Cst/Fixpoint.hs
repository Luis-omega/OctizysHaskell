{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Octizys.Cst.Fixpoint where
import Control.Category ((<<<))
import Data.Coerce (coerce)

newtype Fixpoint f = Fixpoint' { fixpoint:: f (Fixpoint f) }

instance (Show (f (Fixpoint f))) => Show (Fixpoint f) where
  show (Fixpoint' x) = show x

data EF t = EInt Int | Add t t
  deriving (Show,Functor)


type E = Fixpoint EF

-- Definición de RealE
data RealE = REInt Int | RAdd RealE RealE
  deriving Show

cata :: (Functor f) => (f a -> a) -> Fixpoint f -> a
cata f = f <<< (cata f <$> ) <<< fixpoint

-- Función de conversión usando coerce
to :: E -> RealE
to = cata trans
  where
    trans (EInt x) = REInt x
    trans (Add x y) = RAdd x y

type E2 = Fixpoint End

newtype End a = End' {out :: Either String (EF a)}
  deriving Functor

toE :: E2 -> Either [String] RealE
toE = cata (trans <<< coerce)
  where
    trans (Left x ) = Left [x]
    trans (Right (EInt x)) = Right $ REInt x
    trans (Right (Add x y)) =
      case (x,y) of
        (Left x2, Left y2) -> Left (x2<>y2)
        (_ , Left y2) -> Left y2
        (Left x2, _) -> Left x2
        (Right x2, Right y2) -> Right $ RAdd x2 y2

type E3 = Fixpoint End2

newtype End2 a = End2' {out2 :: EF (Either String a)}
  deriving Functor

toEE :: E3 -> Either [String] RealE
toEE = cata (trans <<< out2)
  where
    trans :: EF (Either String (Either [String] RealE)) -> Either [String] RealE
    trans  j = undefined
    -- trans  (Left s) = [s]
    -- trans  (EInt x) = Right (REInt x)
    -- trans  (Add x y) =
    --   case (x,y) of
    --     (Left x2, Left y2) -> Left (x2<>y2)
    --     (_ , Left y2) -> Left y2
    --     (Left x2, _) -> Left x2
    --     (Right x2, Right y2) -> Right $ RAdd x2 y2
