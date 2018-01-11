-- StringSpec.hs ---

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

module StringSpec where

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
    -> Text
    -> Fitness
fit target (Text s) = fromIntegral . sum . map ((flip (^)) 2 . uncurry subtract . bimap ord ord) $ zip s target

born :: (HasStdGen s,
        MonadState s m)
  => Target
  -> Char
  -> Char
  -> m Text
born target minChar maxChar = do
  codes <- randomRepeat (length target) (randomBetween (ord minChar) (ord maxChar))
  pure $ Text (map chr codes)

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

testHelloWorld :: Target -> (Int -> Text -> IO ()) -> IO [Text]
testHelloWorld target f = TextState <$> newStdGen >>= evalStateT (runBiology f card (fit target) (born target lowerChar upperChar) (mutate lowerChar upperChar) (combine target) 0.04 0 100)
  where
    lowerChar = '\x0'
    upperChar = '\x255'
