-- Types.hs ---

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

module Generitic.Types where

import           Control.Lens
import           Control.Monad.State.Strict
import           System.Random

type PopulationSize = Int
type FitnessLimit = Float
type Percentage = Float
type Fitness = Float
type Cardinality = Int

type Fit a = a -> Fitness
type Card a = a -> Cardinality
type Born a = forall s m. (HasStdGen s, MonadState s m) => m a
type Mutate a = forall s m. (HasStdGen s, MonadState s m) => a -> m a
type Combine a = forall s m. (HasStdGen s, MonadState s m) => a -> a -> m a

class HasStdGen s where
  stdGen :: Lens' s StdGen
