#!/usr/bin/env stack
-- stack script --resolver=lts-10.4 --package checkers --package QuickCheck 

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import IfCxt
import Data.Typeable
import Debug.Trace
import Data.Traversable (fmapDefault)
import Data.Functor.Identity
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

mkIfCxtInstances ''Show

data V a = V a

instance forall a. IfCxt (Show a) => Show (V a) where
    show (V a) = ifCxt (Proxy::Proxy (Show a))
        (show a)
        "<<unshowable>>"

data S n a =
  S (n a)
    a
  deriving (Show)

instance (Eq a, Eq (n a)) => Eq (S n a) where
  (S a b) == (S c d) = a == c && b == d

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Functor n) => forall a. Show a => Functor (S n) where
  --fmap :: (Show a, Show b) => (a -> b) -> S n a -> S n b
  fmap f s@(S x y) =
    trace "fmap" $
    traceShow y $
    traceShow (f y)
    S (fmap f x) (f y)

instance Foldable n => Foldable (S n) where
  foldMap f (S x y) = foldMap f x <> f y

instance Traversable n => Traversable (S n) where
  traverse f (S x y) = S <$> traverse f x <*> f y

instance (Show a, Show (n a),
          Applicative n, Testable (n Property), EqProp a) =>
         EqProp (S n a) where
  (S x y) =-= (S p q) =
    trace "=-=" $
    trace "x:" $
    traceShow x $
    trace "y:" $
    traceShow y $
    trace "p:" $
    traceShow p $
    trace "q:" $
    traceShow q $
    (property $ (=-=) <$> x <*> p)
                        -- .&.
                        -- (y =-= q)

-- let s = S [-8,10,-7,1,8,-7,-9,-1] (-4)
-- let f = (Sum . (+3))
-- does this hold: `fmap f s == fmapDefault f s`?
--
-- fmapDefault f ≡ runIdentity . traverse (Identity . f)
--
-- fmap (Sum . (+3)) (S [-8,10,-7,1,8,-7,-9,-1] (-4))
-- == (Functor (S n))
-- S (fmap (Sum . (+3)) [-8,10,-7,1,8,-7,-9,-1]) ((Sum . (+3)) (-4))
-- == (reduce)
-- S [Sum {getSum = -5},Sum {getSum = 13},Sum {getSum = -4},Sum {getSum = 4},Sum {getSum = 11},Sum {getSum = -4},Sum {getSum = -6},Sum {getSum = 2}] (Sum {getSum = -1})
-- 
-- fmapDefault (Sum . (+3)) (S [-8,10,-7,1,8,-7,-9,-1] (-4))
-- ==
-- runIdentity (traverse (Identity . (Sum . (+3))) (S [-8,10,-7,1,8,-7,-9,-1] (-4)))
-- == Traversable (S n)
-- runIdentity (S <$> traverse (Identity . (Sum . (+3))) [-8,10,-7,1,8,-7,-9,-1] <*> (Identity . (Sum . (+3))) (-4))
-- == (eval `Identity . (Sum . (+3))) (-4)`)
-- runIdentity (S <$> traverse (Identity . (Sum . (+3))) [-8,10,-7,1,8,-7,-9,-1] <*> Identity (Sum {getSum = -1}))
-- == (eval `traverse (Identity . (Sum . (+3))) [-8,10,-7,1,8,-7,-9,-1]`)
-- runIdentity (S <$> Identity [Sum {getSum = -5},Sum {getSum = 13},Sum {getSum = -4},Sum {getSum = 4},Sum {getSum = 11},Sum {getSum = -4},Sum {getSum = -6},Sum {getSum = 2}] <*> Identity (Sum {getSum = -1}))
-- ==
-- S [Sum {getSum = -5},Sum {getSum = 13},Sum {getSum = -4},Sum {getSum = 4},Sum {getSum = 11},Sum {getSum = -4},Sum {getSum = -6},Sum {getSum = 2}] (Sum {getSum = -1})

main = do
  let s = S [-3,3,-3] (-4)
  let f = (Sum . (+3))
  print (fmap f s)
  print (fmapDefault f s)
  let trigger8 :: S [] (Int, Int, [Int])
      trigger8 = undefined
  quickBatch (traversable trigger8)
