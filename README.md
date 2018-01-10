# Generitic

This is a genetic framework made after watching the impressive speech of [**Charlie Koster**](https://www.infoq.com/presentations/genetic-algorithms)

The basic idea is:

1. Evolve the generation by mutating 1~7% of the genes
2. Naturally select the best (scored by the `fitness` function) 50% of the generation
3. Combine the best to reproduce the 50% squashed before
4. If the best specimen is below the threshold you defined, end of the game,
otherwise, go back to step 1.

All the stuff is based on this simple interface:

```haskell
type PopulationSize = Int
type FitnessLimit = Float
type Percentage = Float
type Fitness = Float
type Cardinality = Int
type RandomState = StateT StdGen IO

class Mutable a where
  cardinality :: a -> Cardinality
  born :: RandomState a
  mutate :: a -> RandomState a
  combine :: a -> a -> RandomState a
  fitness :: a -> Fitness

runBiology :: (Mutable a)
               => (Int -> a -> IO ())
               -> Percentage
               -> FitnessLimit
               -> Int
               -> RandomState [a]
```

## Example

An example is available under the test directory, it's a string based evolution, mutating until it finds "Hello World"

```haskell
{-# LANGUAGE TupleSections #-}

module StringSpec where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Char
import           Generitic.Evolution
import           Generitic.Types
import           System.IO.Unsafe
import           System.Random

data Text = Text String deriving (Eq, Ord)

targetText = "Hello World !"
minChar = '\x0'
maxChar = '\x255'

instance Mutable Text where

--- Initial generation behavior
  born = do
    codes <- randomRepeat (length targetText) (randomBetween (ord minChar) (ord maxChar))
    pure $ Text (map chr codes)

--- Combination behavior
  combine (Text a) (Text b) = do
    let c = [a, b]
    idx <- randomRepeat (length targetText) (randomBetween 0 2)
    pure $ Text $ map (\(i, j) -> c !! i !! j) (zip idx [0..])

--- Mutation behavior
  mutate (Text x) = do
    idx <- randomBetween 0 (length x)
    mut <- randomBetween (ord minChar) (ord maxChar)
    pure $ Text (x & ix idx .~ chr mut)

--- Cardinality of our specy, defining how much genes it has
  cardinality x = length targetText

--- The fitness gives the distance between the desired state and our current mutation
  fitness (Text s) = fromIntegral . sum . map ((flip (^)) 2 . uncurry subtract . bimap ord ord) $ zip s targetText

instance Show Text where
  show (Text x) = show x

testHelloWorld :: (Int -> Text -> IO ()) -> IO [Text]
--- 0.03 = 3% of mutation
--- 0 = fitness limit, we should be <= this limit to end
--- 200 = generation size (how much specimens)
testHelloWorld f = newStdGen >>= evalStateT (runBiology f 0.03 0 200)
```
