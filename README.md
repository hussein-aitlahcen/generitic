# Generitic

This is a genetic framework made after watching the impressive speech of [**Charlie Koster**](https://www.infoq.com/presentations/genetic-algorithms)

The basic idea is:

1. Evolve the generation by mutating 1~7% of the genes.
2. Naturally select the best (scored by the `fitness` function) 50% of the generation.
3. Combine the best to reproduce the 50% squashed before.
4. If the best specimen is below the threshold you defined, end of the game,
otherwise, go back to step 1.

All the stuff is based on this simple interface:

```haskell
{-# LANGUAGE RankNTypes #-}

import           Control.Lens
import           Control.Monad.State.Strict
import           System.Random

type PopulationSize = Int
type FitnessLimit = Float
type Percentage = Float
type Fitness = Float
type Cardinality = Int
type GenerationNumber = Int

type Callback a = GenerationNumber -> a -> IO ()
type Fit a = a -> Fitness
type Card a = a -> Cardinality
type Born a = forall s m. (HasStdGen s, MonadState s m) => m a
type Mutate a = forall s m. (HasStdGen s, MonadState s m) => a -> m a
type Combine a = forall s m. (HasStdGen s, MonadState s m) => a -> a -> m a

class HasStdGen s where
  stdGen :: Lens' s StdGen

runBiology :: (HasStdGen s, MonadState s m, MonadIO m)
           => Callback
           -> Card a
           -> Fit a
           -> Born a
           -> Mutate a
           -> Combine a
           -> Percentage
           -> FitnessLimit
           -> Int
           -> m [a]
```

## Example

An example is available under the test directory, it's a string based evolution, mutating until it finds the **target** text.

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Char
import           Generitic.Evolution
import           Generitic.Types
import           System.Random

type Target = String

data Text = Text Target deriving (Eq, Ord)

instance Show Text where
  show (Text x) = show x

data TextState = TextState { _stdGen :: StdGen }

instance HasStdGen TextState where
  stdGen = lens _stdGen $ \a b -> a { _stdGen = b }

card :: Text -> Cardinality
card (Text s) = length s

fit :: Target
    -> Fit Text
fit target (Text s) = fromIntegral . sum . map (squareFit . subtractTuple . toCharCodes) $ zipWithTarget s
  where
    zipWithTarget = zip target
    toCharCodes = bimap ord ord
    subtractTuple = uncurry subtract
    squareFit = (flip (^)) 2

born :: (HasStdGen s,
        MonadState s m)
  => Target
  -> Char
  -> Char
  -> m Text
born target minChar maxChar = Text . map chr <$> codes
  where
    codes = randomRepeat (length target) (randomBetween (ord minChar) (ord maxChar))

combine :: (HasStdGen s,
           MonadState s m)
        => Target
        -> Text
        -> Text
        -> m Text
combine target (Text a) (Text b) = do
  let c = [a, b]
  idx <- randomRepeat (length target) (randomBetween 0 2)
  pure $ Text $ map (\(i, j) -> c !! i !! j) (zip idx [0..])

mutate :: (HasStdGen s,
          MonadState s m)
       => Char
       -> Char
       -> Text
       -> m Text
mutate minChar maxChar (Text x) = do
  idx <- randomBetween 0 (length x)
  mut <- randomBetween (ord minChar) (ord maxChar)
  pure $ Text (x & ix idx .~ chr mut)

testHelloWorld :: Target -> Callback Text -> IO [Text]
testHelloWorld target f = TextState <$> newStdGen >>= evalStateT (runBiology f card (fit target) (born target lowerChar upperChar) (mutate lowerChar upperChar) (combine target) 0.04 0 100)
  where
    lowerChar = '\x0'
    upperChar = '\x255'
```
