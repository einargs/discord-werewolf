module Werewolf.Start
  (
  ) where

import Werewolf.Player
import Werewolf.Game (Game(..))
import Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels)

import Control.Monad.Random

randomRole :: MonadRandom m => m Role
randomRole = toEnum <$> getRandomR (mini, maxi)
  where
    maxi = fromEnum (maxBound :: Role)
    mini = fromEnum (minBound :: Role)

data SetupEvent
  = AddPlayer PlayerName
  | StartGame

data PreGame = PreGame { players :: [PlayerName] }
  deriving (Show, Eq)
makeFieldLabelsWith noPrefixFieldLabels ''PreGame

