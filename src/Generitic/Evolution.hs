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

module Generitic.Evolution where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.List
import           Data.Ord
import           Generitic.Types
import qualified System.Random              as R

sortDNA :: Mutable a => [a] -> [a]
sortDNA = sortBy compareDNA
  where
    compareDNA a b = compare (fitness a) (fitness b)

split :: [a] -> ([a], [a])
split l = splitAt (length l `div` 2) l

randomBetween :: Int -> Int -> RandomState Int
randomBetween lower upper = do
  gen <- get
  let (value, nextGen) = R.randomR (0, 1) gen :: (Float, R.StdGen)
  put nextGen
  pure $ lower + (round $ fromIntegral (upper - 1 - lower) * value)

randomRepeat :: Int -> RandomState a -> RandomState [a]
randomRepeat i f = replicateM i f

createGeneration :: Mutable a => PopulationSize -> RandomState [a]
createGeneration size = replicateM size born

numberOfMutations :: (Mutable a) => Percentage -> [a] -> Int
numberOfMutations percent generation = round $ fromIntegral totalCardinality * percent
  where totalCardinality = sum . map cardinality $ generation

mutationIndexes :: Int -> Int -> Int -> RandomState [Int]
mutationIndexes lower upper nbOfMut = do
  idx <- randomRepeat nbOfMut (randomBetween lower upper)
  pure $ take nbOfMut idx

mutateRandomly :: (Mutable a) => Percentage -> [a] -> RandomState [a]
mutateRandomly percent generation = do
  let nbOfMut = numberOfMutations percent generation
  mutIdx <- mutationIndexes 0 (length generation) nbOfMut
  foldM (\acc i -> element i mutate acc) generation mutIdx

createChildrens :: Mutable a => [a] -> RandomState [a]
createChildrens survivors = mapM (uncurry combine) (zip survivors reversed)
  where
    reversed = reverse survivors

naturalSelection :: (Mutable a) => [a] -> RandomState [a]
naturalSelection generation = do
  let (survivors, _) = split . sortDNA $ generation
  childs <- createChildrens survivors
  pure $ survivors ++ childs

biologicEvolution :: (Mutable a)
                  => (Int -> a -> IO ())
                  -> Percentage
                  -> FitnessLimit
                  -> Int
                  -> [a]
                  -> RandomState [a]
biologicEvolution f mutPercent limit generationNumber generation = do
  generation' <- evolve generation mutPercent
  liftIO $ f generationNumber $ minimumBy (comparing fitness) generation'
  if any bellowFitness generation'
    then pure $ sortDNA generation'
    else biologicEvolution f mutPercent limit generationNumber' generation'
  where
    bellowFitness = (>=) limit . fitness
    generationNumber' = generationNumber + 1
    evolve generation percent = naturalSelection generation >>= mutateRandomly percent

runBiology :: (Mutable a)
               => (Int -> a -> IO ())
               -> Percentage
               -> FitnessLimit
               -> Int
               -> RandomState [a]
runBiology f percent limit generationSize = do
  initialGeneration <- createGeneration generationSize
  biologicEvolution f percent limit 0 initialGeneration
