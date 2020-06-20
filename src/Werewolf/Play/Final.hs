module Werewolf.Play.Final
  ( playGame
  ) where

import Control.Monad.Random
import Control.Monad.State.Class
import Data.List ((\\))
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
  -- Remove the Hexed effect at the beginning of the day.
  removeHexed
  withTimer AccusationsAllowedTimer $ handleDayAction UnanimousAccusations
  withTimer NightEndTimer $ handleDayAction MajorityAccusations

night :: MonadWG m => m ()
night = do
  -- Transition from day to night
  dayToNight
  -- Get a list of the players alive at the start of the night
  -- so we can figure out who died at the end of the night.
  playersAtStart <- view #name <$> queryPlayers []
  -- Remove the protected status from all players, as protectors will soon choose
  -- new wards.
  removeProtected
  -- Perform role actions
  nightlyActions
  -- At the end of the night, clear the action buffer.
  clearActionBuffer
  -- Figure out who died during the night
  playersAtEnd <- view #name <$> queryPlayers []
  let deadPlayers = playersAtEnd \\ playersAtStart
  announce $ T.concat
    [ "During the night, these players died: "
    , T.intercalate " " deadPlayers ]
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
