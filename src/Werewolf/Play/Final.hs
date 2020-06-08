module Werewolf.Play.Final
  ( playGame
  ) where

import Control.Monad.Random
import Control.Monad.State.Class

import Optics.State.Operators ((%=), (.=))

import Werewolf.Play.Base
import Werewolf.Game
import Werewolf.Player
import Werewolf.Play.Roles

-- The first night.
firstNight :: (MonadWG m) => m ()
firstNight = do
  -- Transition to night; the event queue should empty right now.
  dayToNight
  -- Perform the first night role actions
  firstNightActions
  -- End the first setup round
  endRound

gameStart :: MonadWG m => m ()
gameStart = do
  announce "The game has started."
  firstNight

day :: MonadWG m => m ()
day = do
  removeHexed

night :: MonadWG m => m ()
night = do
  -- Transition from day to night
  dayToNight
  -- Perform role actions
  nightlyActions
  -- At the end of the night, clear the action buffer.
  clearActionBuffer
  endRound

-- |
roundLoop :: MonadWG m => m Victory
roundLoop = do
  day
  night
  roundLoop

-- | This is the action that a session should be used to run.
playGame :: MonadWG m => m Victory
playGame = do
  gameStart
  roundLoop
