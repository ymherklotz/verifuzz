-- Module      : Verismith.Verilog2005.Randomness
-- Description : Random sources
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : stable
-- Portability : POSIX
{-# LANGUAGE ScopedTypeVariables #-}

module Verismith.Verilog2005.Randomness
  ( shuffle,
    sampleCategoricalProbability,
    sampleNumberProbability,
    sampleIn,
    sampleInString,
    sampleBernoulli,
    choice,
    sampleMaybe,
    sampleEither,
    sampleSegment,
    sampleEnum,
    sampleMaybeEnum,
    sampleWeighted,
    sampleFrom,
    sampleFromString,
    sampleBranch,
    sampleNum,
    sampleN,
    sampleNE,
    sampleString,
    sampleNEString,
    sampleFiltered,
    GenM,
  )
where

import Control.Applicative (liftA2)
import Control.Monad (join, replicateM, forM_)
import Control.Monad.Reader
import qualified Data.ByteString as B
import Data.List
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.List.NonEmpty as NE
import Control.Monad.Primitive (PrimMonad, PrimState, RealWorld)
import Data.Word
import Data.Bifunctor (first, second)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VM
import System.Random.MWC.Probability
import Verismith.Config (CategoricalProbability (..), NumberProbability (..), uniformCP)
import Verismith.Utils (nonEmpty, foldrMap1)

infixl 4 <.>

(<.>) :: (Monad m, Applicative m) => m (a -> m b) -> m a -> m b
(<.>) mf mx = join $ mf <*> mx

avoid :: [Int] -> Int -> Int
avoid l x = case l of
  h : t | h <= x -> avoid t $ x + 1
  _ -> x

uniq :: Ord b => (a -> b) -> (a -> a -> a) -> [a] -> [a]
uniq f m =
  nonEmpty [] $ toList
    . foldrMap1 (:|[]) (\e (x :| a) -> if f x == f e then (m x e) :| a else e :| x : a)
    . NE.sortWith f

clean :: Int -> [(Double, Int)] -> [(Double, Int)]
clean t =
  map (first $ max 0)
    . uniq snd (\(x1, y1) (x2, y2) -> (x1 + x2, y1))
    . filter ((<= t) . snd)

shuffle :: (PrimMonad m, VM.Unbox x) => Gen (PrimState m) -> [x] -> m [x]
shuffle gen l = do
  let n = length l - 1
  v <- VU.thaw $ VU.fromList l
  forM_ [0..n] $ \i -> sample (uniformR (i, n)) gen >>= VM.swap v i
  VU.toList <$> VU.unsafeFreeze v

sampleCategoricalProbability ::
  PrimMonad m => Int -> Gen (PrimState m) -> CategoricalProbability -> m Int
sampleCategoricalProbability t gen d = case d of
  CPDiscrete l ->
    let ll = NE.take (t + 1) l
     in case ll of
          [] -> error "Probability vector cannot be empty"
          [x] -> pure 0
          _ -> sample (categorical ll) gen
  CPBiasedUniform l b ->
    let ll = clean t l
        uw = fromIntegral (t + 1 - length ll) * b
     in nonEmpty
          (pure Nothing)
          (flip sample gen . discrete . ((uw, Nothing) :) . map (second Just) . toList)
          ll
          >>= maybe (avoid (map snd ll) <$> sample (uniformR (0, t - length ll)) gen) pure

sampleNumberProbability :: PrimMonad m => Gen (PrimState m) -> NumberProbability -> m Int
sampleNumberProbability gen d = case d of
  NPUniform l h -> sample (uniformR (l, h)) gen
  NPBinomial o t f -> (o +) <$> sample (binomial t f) gen
  NPNegativeBinomial o r f -> (o +) <$> sample (negativeBinomial f r) gen
  NPPoisson o p -> (o +) <$> sample (poisson p) gen
  NPDiscrete l -> sample (discrete l) gen
  NPLinearComb l -> sample (discrete l) gen >>= sampleNumberProbability gen

sampleIn :: (Functor m, PrimMonad m) => [a] -> Gen (PrimState m) -> CategoricalProbability -> m a
sampleIn l gen d = (l !!) <$> sampleCategoricalProbability (length l - 1) gen d

sampleInString ::
  (Functor m, PrimMonad m) =>
  B.ByteString ->
  Gen (PrimState m) ->
  CategoricalProbability ->
  m Word8
sampleInString s gen d = (B.index s) <$> sampleCategoricalProbability (B.length s - 1) gen d

type GenM p = ReaderT (p, Gen RealWorld) IO

sampleWrapper :: (p -> d) -> (Gen RealWorld -> d -> GenM p x) -> GenM p x
sampleWrapper p f = f <$> asks snd <.> asks (p . fst)

sampleBernoulli :: (p -> Double) -> GenM p Bool
sampleBernoulli p = sample <$> (bernoulli <$> asks (p . fst)) <.> asks snd

choice :: (p -> Double) -> GenM p a -> GenM p a -> GenM p a
choice c t f = sampleBernoulli c >>= \b -> if b then t else f

sampleMaybe :: (p -> Double) -> GenM p a -> GenM p (Maybe a)
sampleMaybe c x = choice c (Just <$> x) (pure Nothing)

sampleEither :: (p -> Double) -> GenM p a -> GenM p b -> GenM p (Either a b)
sampleEither c t f = choice c (Left <$> t) (Right <$> f)

sampleSegment :: (p -> CategoricalProbability) -> Int -> Int -> GenM p Int
sampleSegment p l h = (l +) <$> (sampleWrapper p $ sampleCategoricalProbability $ h - l)

sampleEnum :: forall a p. (Bounded a, Enum a) => (p -> CategoricalProbability) -> GenM p a
sampleEnum p = toEnum <$> sampleSegment p (fromEnum (minBound :: a)) (fromEnum (maxBound :: a))

sampleMaybeEnum ::
  forall a p. (Bounded a, Enum a) => (p -> CategoricalProbability) -> GenM p (Maybe a)
sampleMaybeEnum p =
  (\n -> if n == 0 then Nothing else Just $ toEnum $ mib + n - 1)
    <$> (sampleWrapper p $ sampleCategoricalProbability $ mab - mib + 1)
  where
    mib = fromEnum (minBound :: a)
    mab = fromEnum (maxBound :: a)

sampleWeighted :: [(Double, a)] -> GenM p a
sampleWeighted l = case l of
  [] -> error "Probability vector cannot be empty"
  [(_, x)] -> pure x
  _ -> asks snd >>= sample (discrete l)

sampleFrom :: (p -> CategoricalProbability) -> [a] -> GenM p a
sampleFrom p l = sampleWrapper p $ sampleIn l

sampleFromString :: (p -> CategoricalProbability) -> B.ByteString -> GenM p Word8
sampleFromString p s = sampleWrapper p $ sampleInString s

sampleBranch :: (p -> CategoricalProbability) -> [GenM p a] -> GenM p a
sampleBranch p l = join $ sampleFrom p l

sampleNum :: (p -> NumberProbability) -> GenM p Int
sampleNum p = sampleWrapper p sampleNumberProbability

sampleN :: (p -> NumberProbability) -> GenM p b -> GenM p [b]
sampleN p x = sampleNum p >>= flip replicateM x

sampleNE :: (p -> NumberProbability) -> GenM p b -> GenM p (NonEmpty b)
sampleNE p x = liftA2 (:|) x $ sampleN p x

sampleString ::
  (p -> NumberProbability) -> (p -> CategoricalProbability) -> B.ByteString -> GenM p B.ByteString
sampleString np cp s = B.pack <$> sampleN np (sampleFromString cp s)

sampleNEString ::
  (p -> NumberProbability) -> (p -> CategoricalProbability) -> B.ByteString -> GenM p B.ByteString
sampleNEString np cp s = B.pack . toList <$> sampleNE np (sampleFromString cp s)

deleteFirstOrdered :: Ord c => (a -> c) -> (b -> c) -> [a] -> [b] -> [a]
deleteFirstOrdered pa pb la lb = case (la, lb) of
  (ha : ta, hb : tb) -> case compare (pa ha) (pb hb) of
    LT -> ha : deleteFirstOrdered pa pb ta lb
    EQ -> deleteFirstOrdered pa pb ta tb
    GT -> deleteFirstOrdered pa pb la tb
  _ -> la

merge :: Ord a => [a] -> [a] -> [a]
merge la lb = case (la, lb) of
  (ha : ta, hb : tb) -> case compare ha hb of
    LT -> ha : merge ta lb
    EQ -> ha : hb : merge ta tb
    GT -> hb : merge la tb
  (_, []) -> la
  _ -> lb

sampleFiltered :: (p -> CategoricalProbability) -> Int -> [Int] -> GenM p Int
sampleFiltered p t l = do
  gen <- asks snd
  d <- asks $ p . fst
  case d of
    CPDiscrete l ->
      avoid ll
        <$> sample (discrete $ deleteFirstOrdered snd id (zip (NE.take (t + 1) l) [0 .. t]) ll) gen
    CPBiasedUniform l' b ->
      let ll' = deleteFirstOrdered snd id (clean t l') ll
          uw = fromIntegral (t - length ll - length ll') * b
       in sample (discrete $ (uw, Nothing) : map (second Just) ll') gen
            >>= maybe
              ( avoid (merge ll $ map snd ll')
                  <$> sample (uniformR (0, t - length ll - length ll')) gen
              )
              pure
  where
    ll = sort $ filter (<= t) l
