{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sweeper.Grid.BalancedTernary (Stream, index, Index, toIndex, fromIndex, randomStream) where

import Data.Coerce
import System.Random

data Trit = T | O | I
  deriving (Eq, Ord, Show)

predTernary :: [Trit] -> [Trit]
predTernary [] = [T]
predTernary [I] = []
predTernary (O:ns) = T:ns
predTernary (I:ns) = O:ns
predTernary (T:ns) = I:predTernary ns

succTernary :: [Trit] -> [Trit]
succTernary [] = [I]
succTernary [T] = []
succTernary (T:ns) = O:ns
succTernary (O:ns) = I:ns
succTernary (I:ns) = T:succTernary ns

consO :: [Trit] -> [Trit]
consO [] = []
consO xs = O:xs

plusTernary :: [Trit] -> [Trit] -> [Trit]
plusTernary [] ns = ns
plusTernary ms [] = ms
plusTernary (T:ms) (O:ns) = T:plusTernary ms ns
plusTernary (T:ms) (I:ns) = consO $ plusTernary ms ns
plusTernary (O:ms) (O:ns) = consO $ plusTernary ms ns
plusTernary (O:ms) (n:ns) = n:plusTernary ms ns
plusTernary (I:ms) (T:ns) = consO $ plusTernary ms ns
plusTernary (I:ms) (O:ns) = I:plusTernary ms ns
plusTernary (T:ms) (T:ns) = predTernary $ T:plusTernary ms ns
plusTernary (I:ms) (I:ns) = succTernary $ I:plusTernary ms ns

toBalancedTernary :: Integer -> [Trit]
toBalancedTernary 0 = []
toBalancedTernary x = case x `divMod` 3 of
  (q, 0) -> O : toBalancedTernary q
  (q, 1) -> I : toBalancedTernary q
  (q, 2) -> T : toBalancedTernary (q + 1)
  _ -> error "Unreachable"

fromBalancedTernary :: [Trit] -> Integer
fromBalancedTernary [] = 0
fromBalancedTernary (T:ns) = (-1) + 3 * fromBalancedTernary ns
fromBalancedTernary (O:ns) =        3 * fromBalancedTernary ns
fromBalancedTernary (I:ns) =   1  + 3 * fromBalancedTernary ns

newtype Index = Index {getIndex :: [Trit]} -- invariant no trailing zeros
  deriving (Eq, Ord)

instance Semigroup Index where
  (<>) = coerce plusTernary

instance Monoid Index where
  mempty = Index []

instance Enum Index where
  succ = coerce succTernary
  pred = coerce predTernary
  fromEnum = fromInteger . fromIndex
  toEnum = toIndex . toInteger

toIndex :: Integer -> Index
toIndex = coerce toBalancedTernary

fromIndex :: Index -> Integer
fromIndex = coerce fromBalancedTernary

-------------------------------------------------------------------------------

data Nat = Z | S Nat

data SNat n where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

data TernaryTree n a where
  Leaf :: a -> TernaryTree 'Z a
  Branch :: TernaryTree n a -> TernaryTree n a -> TernaryTree n a -> TernaryTree ('S n) a

data CoTernaryTree n a where
  CoTernaryTree :: TernaryTree n a -> CoTernaryTree ('S n) a -> TernaryTree n a -> CoTernaryTree n a

-- | Skew balanced ternary skip stream
data Stream a = Stream a (CoTernaryTree 'Z a)

data Vec n a where
  Nil :: Vec 'Z a
  Cons :: a -> Vec n a -> Vec ('S n) a

indexTernaryTree :: TernaryTree n a -> Vec n Trit -> a
indexTernaryTree (Leaf a) Nil = a
indexTernaryTree (Branch t o i) (Cons x n) = case x of
  T -> indexTernaryTree t n
  O -> indexTernaryTree o n
  I -> indexTernaryTree i n

indexCoTernaryTree :: CoTernaryTree n a -> Vec n Trit -> [Trit] -> a
indexCoTernaryTree (CoTernaryTree _ _ _) _ [] = error "Unreachable"
indexCoTernaryTree (CoTernaryTree t _ i) v [x] = case x of
  T -> indexTernaryTree t v
  O -> error "Unreachable"
  I -> indexTernaryTree i v
indexCoTernaryTree (CoTernaryTree _ o _) v (x:xs) = indexCoTernaryTree o (Cons x v) xs

indexSBTSS :: Stream a -> [Trit] -> a
indexSBTSS (Stream a _) [] = a
indexSBTSS (Stream _ b) xs = indexCoTernaryTree b Nil xs

index :: Stream a -> Index -> a
index s = indexSBTSS s . getIndex

randomStream :: forall a. (StdGen -> (a, StdGen)) -> StdGen -> Stream a
randomStream f gen =
  let (a, gen') = f gen
  in Stream a $ randomCoTernaryTree gen' SZ
  where
    randomCoTernaryTree :: StdGen -> SNat n -> CoTernaryTree n a
    randomCoTernaryTree g n =
      let (g0, g1) = split g
          (g2, g3) = split g0
      in CoTernaryTree (randomTernaryTree g1 n) (randomCoTernaryTree g2 (SS n)) (randomTernaryTree g3 n)

    randomTernaryTree :: StdGen -> SNat n -> TernaryTree n a
    randomTernaryTree g SZ = Leaf . fst $ f g
    randomTernaryTree g (SS n) =
      let (g0, g1) = split g
          (g2, g3) = split g0
      in Branch (randomTernaryTree g1 n) (randomTernaryTree g2 n) (randomTernaryTree g3 n)
