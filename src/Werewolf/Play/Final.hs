module Werewolf.Play.Final
  (
  ) where

import Control.Monad.Random
import Control.Monad.State.Class

import Optics.State.Operators ((%=), (.=))

import Werewolf.Play.Base
import Werewolf.Game
import Werewolf.Player
import qualified Werewolf.Play.Roles as Roles
import Werewolf.Play.Roles (killPlayer)

-- The first night.
firstNight :: (MonadWG m) => m ()
firstNight = do
  -- Transition to night; the event queue should empty right now.
  dayToNight
  -- End the first setup round
  endRound

gameStart :: MonadWG m => m ()
gameStart = do
  announce "The game has started."
  firstNight

day :: MonadWG m => m ()
day = pure ()

night :: MonadWG m => m ()
night = do
  -- Transition from day to night
  dayToNight
  -- Begin performing role actions
  Roles.mystic
  -- TODO: all the other roles
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
