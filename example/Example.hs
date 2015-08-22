{-# LANGUAGE
    TypeFamilies
  , TypeOperators
  , DataKinds
  , PolyKinds
  , ConstraintKinds
  , KindSignatures
  , AllowAmbiguousTypes
  #-}

module Example where

import Data.Function.Poly
import Data.HList


type family (xs :: [k]) :++ (ys :: [k]) :: [k] where
  '[]       :++ ys = ys
  (x ': xs) :++ ys = x ': xs :++ ys

foo :: ( TypeListToArity xs r ~ f -- Proves the arity - list isomorphism
       , ArityToTypeList f ~ (:++) xs (r ': '[])
       ) => f -> f
foo = id

bar :: ( ArityMinusTypeList f xs ~ r -- Proves replacement
       , ExpectArity xs f
       ) => f -> f
bar = id


xs :: HList (Integer ': Bool ': Maybe () ': '[])
xs = HCons (5 :: Integer) $ HCons True $ HCons (Just ()) HNil


baz :: Integer -> Bool -> Maybe () -> String -> ()
baz _ _ _ _ = ()
