module Werewolf.Game
  () where

import Control.Monad.Random
import Control.Monad.State.Class

import Data.List (null)

import Werewolf.Player

data Victory
  = WerewolfVictory
  | VillagerVictory
  | MonsterVictory

data Event
  = PlayerAction Action
  | DayEnd
  | NightEnd
  | PM PlayerName String
  | Announce String
  | Victory Victory
  deriving (Show, Eq)

data Action = Action PlayerName ActionInfo
  deriving (Show, Eq)

data ActionInfo
  = Accuse PlayerName
  | LynchVote Bool
  | Lynch PlayerName
  | WerewolfKill PlayerName
  | SpellcasterHex PlayerName
  | DoctorRevive PlayerName
  | SeerClairvoyance PlayerName
  | BodyguardProtect PlayerName
  | GuardianAngelProtect PlayerName
  | HuntressKill PlayerName
  | HarlotHideWith PlayerName
  | HunterRevenge PlayerName
  | MentalistCompare PlayerName PlayerName
  | MadScientistExplode PlayerName PlayerName
  | CupidArrow PlayerName PlayerName
  | MysticCount
  | ProphetVision Role
  | RevealerKill PlayerName PlayerName
  | MasonReveal
  | GunnerShoot PlayerName
  | DoppelgangerChoose PlayerName
  | DoppelgangerBecome PlayerName
  | TurncoatSwitch Team
  deriving (Show, Eq)

data Phase = Day | Night
  deriving (Show, Eq)

-- | Gives the phases that an action can be taken during.
validPhases :: Action -> [Phase]
validPhases (Action _ info) = case info of
  Accuse _ -> [Day]
  LynchVote _ -> [Day]
  WerewolfKill _ -> [Day]
  HarlotHideWith _ -> [Night]
  SpellcasterHex _ -> [Night]
  DoctorRevive _ -> [Night]
  SeerClairvoyance _ -> [Night]

newtype Round = Round [Event]
  deriving (Show, Eq)

data Game = Game
  { currentPhase :: Phase
  , gamePlayers :: [Player]
  , rounds :: [Round]
  } deriving (Show, Eq)

type MonadWG m = (MonadState Game m, MonadRandom m)

queryPlayers :: (MonadState Game m) => (Player -> Bool) -> m [Player]
queryPlayers q = gets (filter q)

isRoleActive :: (MonadState Game m) => Role -> m Bool
isRoleActive role = (not . null) <$> queryPlayers (hasRole role)

nightStart :: MonadWG m => m ()
nightStart = pure ()

dayStart :: MonadWG m => m ()
dayStart = pure ()

process :: (MonadWG m) => Event -> m ()
process = \case
  DayEnd -> nightStart
  NightEnd -> dayStart
