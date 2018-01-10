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
import           System.IO.Unsafe
import           System.Random

data Text = Text String deriving (Eq, Ord)

targetText = "Hello World !"
minChar = '\x0'
maxChar = '\x255'

instance Mutable Text where
  born = do
    codes <- randomRepeat (length targetText) (randomBetween (ord minChar) (ord maxChar))
    pure $ Text (map chr codes)

  combine (Text a) (Text b) = do
    let c = [a, b]
    idx <- randomRepeat (length targetText) (randomBetween 0 2)
    pure $ Text $ map (\(i, j) -> c !! i !! j) (zip idx [0..])

  mutate (Text x) = do
    idx <- randomBetween 0 (length x)
    mut <- randomBetween (ord minChar) (ord maxChar)
    pure $ Text (x & ix idx .~ chr mut)

  cardinality x = length targetText

  fitness (Text s) = fromIntegral . sum . map ((flip (^)) 2 . uncurry subtract . bimap ord ord) $ zip s targetText

instance Show Text where
  show (Text x) = show x

testHelloWorld :: (Int -> Text -> IO ()) -> IO [Text]
testHelloWorld f = newStdGen >>= evalStateT (runBiology f 0.03 0 200)
