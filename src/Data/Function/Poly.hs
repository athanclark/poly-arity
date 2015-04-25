{-# LANGUAGE
    TypeFamilies
  , KindSignatures
  , DataKinds
  , ConstraintKinds
  , TypeOperators
  , MultiParamTypeClasses
  , FunctionalDependencies
  , PolyKinds
  , FlexibleInstances
  , UndecidableInstances
  #-}

module Data.Function.Poly where

import Data.Constraint
import Data.HList


type family TypeListToArity (xs :: [*]) (r :: *) :: * where
  TypeListToArity '[] r = r -- basis
  TypeListToArity (x ': xs) r = x -> TypeListToArity xs r

type family ExpectArity (xs :: [*]) (f :: *) :: Constraint where
  ExpectArity '[] f = () -- basis
  ExpectArity (x ': xs) (x -> remainder) = ExpectArity xs remainder

type family Head (xs :: [k]) :: k where
  Head (x ': xs) = x

type family Tail (xs :: [k]) :: [k] where
  Tail (x ': xs) = xs


class ExpectArity xs f => ConsumeArity (xs :: [*]) (f :: *) result | xs f -> result where
  appN :: f -> HList xs -> result

instance ConsumeArity '[] r r where
  appN r _ = r

instance ( ConsumeArity xs f r
         , ExpectArity (x ': xs) (x -> f) )=> ConsumeArity (x ': xs) (x -> f) r where
  appN f (HCons x xs) = appN (f x) xs

-- class ConsumeArity (xs :: [*]) (f :: *) result | xs f -> result where
--   appN :: f -> Head xs -> TypeListToArity (Tail xs) result
--
-- instance ConsumeArity (x ': '[]) (x -> r) r where
--   appN f x = f x
--
-- instance ConsumeArity xs rs r => ConsumeArity (x ': xs) (x -> rs) r where
--   appN f x = appN (f x)

--instance ConsumeArity '[] r r where
--  appN
