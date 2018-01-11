-- Evolution.hs ---

-- Copyright (C) 2018 Hussein Ait-Lahcen

-- Author: Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}

module Generitic.Evolution where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.List
import           Data.Ord
import           Generitic.Types
import           System.Random

sortSpecimens :: Fit a
              -> [a]
              -> [a]
sortSpecimens fit generation = sortBy (\a b -> compare (fit a) (fit b)) generation

randomBetween :: (HasStdGen s,
                  MonadState s m)
              => Int
              -> Int
              -> m Int
randomBetween lower upper = do
  gen <- use stdGen
  let (value, nextGen) = randomR (0, 1) gen :: (Float, StdGen)
  stdGen .= nextGen
  pure $ lower + (round $ fromIntegral (upper - 1 - lower) * value)

randomRepeat :: Monad m
             => Int
             -> m a
             -> m [a]
randomRepeat i f = replicateM i f

createGeneration :: (HasStdGen s, MonadState s m)
                 => Born a
                 -> PopulationSize
                 -> m [a]
createGeneration born size = replicateM size born

numberOfMutations :: Card a
                  -> Percentage
                  -> [a]
                  -> Int
numberOfMutations card percent generation = round $ fromIntegral (sum . map card $ generation) * percent

mutationIndexes :: (HasStdGen s,
                  MonadState s m)
                => Int
                -> Int
                -> Int
                -> m [Int]
mutationIndexes lower upper nbOfMut = do
  idx <- randomRepeat nbOfMut (randomBetween lower upper)
  pure $ take nbOfMut idx

mutateRandomly :: (HasStdGen s, MonadState s m)
               => Card a
               -> Mutate a
               -> Percentage
               -> [a]
               -> m [a]
mutateRandomly card mutate percent generation = do
  let nbOfMut = numberOfMutations card percent generation
  mutIdx <- mutationIndexes 0 (length generation) nbOfMut
  foldM (\acc i -> element i mutate acc) generation mutIdx

createChildrens :: (HasStdGen s, MonadState s m)
                => Combine a
                -> [a]
                -> m [a]
createChildrens combine survivors = do
  mapM (uncurry combine) (zip survivors reversed)
  where
    reversed = reverse survivors

naturalSelection :: (HasStdGen s, MonadState s m)
                 => Fit a
                 -> Combine a
                 -> [a]
                 -> m [a]
naturalSelection fit combine generation = do
  let sortedSpecimens = sortSpecimens fit generation
      (survivors, _) = sp sortedSpecimens
  childs <- createChildrens combine survivors
  pure $ survivors ++ childs
  where
    sp l = splitAt (length l `div` 2) l

biologicEvolution :: (HasStdGen s, MonadState s m, MonadIO m)
                  => (Int -> a -> IO ())
                  -> Card a
                  -> Fit a
                  -> Born a
                  -> Mutate a
                  -> Combine a
                  -> Percentage
                  -> FitnessLimit
                  -> Int
                  -> [a]
                  -> m [a]
biologicEvolution callback card fit born mutate combine mutPercent limit generationNumber generation = do
  generation' <- evolve generation mutPercent
  liftIO $ callback generationNumber $ minimumBy (comparing fit) generation'
  if any ((>=) limit . fit) generation'
    then pure $ sortSpecimens fit generation'
    else biologicEvolution callback card fit born mutate combine mutPercent limit generationNumber' generation'
  where
    generationNumber' = generationNumber + 1
    evolve generation percent = naturalSelection fit combine generation >>= mutateRandomly card mutate percent

runBiology :: (HasStdGen s, MonadState s m, MonadIO m)
           => (Int -> a -> IO ())
           -> Card a
           -> Fit a
           -> Born a
           -> Mutate a
           -> Combine a
           -> Percentage
           -> FitnessLimit
           -> Int
           -> m [a]
runBiology callback card fit born mutate combine percent limit generationSize = do
  initialGeneration <- createGeneration born generationSize
  biologicEvolution callback card fit born mutate combine percent limit 0 initialGeneration
