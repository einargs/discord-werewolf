module Werewolf.Game where

import Control.Monad.Random
import Control.Monad.State.Class

import Data.List (null)

import Werewolf.Player

data Victory
  = WerewolfVictory
  | VillagerVictory
  | MonsterVictory

data Message
  = PM PlayerName String
  | Announce String

data Event
  = PlayerAction Action
  | DayEnd
  | NightEnd
  | SendMessage Message
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

class (Monad m) => MonadMessage m where
  sendMessage :: Message -> m ()

type MonadWG m = (MonadState Game m, MonadRandom m, MonadMessage m)

addEvent :: (MonadState Game m) => Event -> m ()
addEvent e = case e of
  NightEnd -> modify $ endNight . appendEvent
  _ -> modify appendEvent
  where
    appendEvent :: Game -> Game
    appendEvent g@Game{rounds} =
      let rounds' = case rounds of
              [] -> [Round [e]]
              (Round events):rest -> (Round (e:events)):rest
      in g{rounds = rounds'}

    endNight :: Game -> Game
    endNight g@Game{rounds} = g{rounds = (Round []):rounds}

-- | Only use this to send messages; it makes sure to log the message
-- in the event log.
handleMessage :: (MonadState Game m, MonadMessage m) => Message -> m ()
handleMessage msg = do
  addEvent $ SendMessage msg
  sendMessage msg

pm :: (MonadState Game m, MonadMessage m) => PlayerName -> String -> m ()
pm pn str = handleMessage $ PM pn str

announce :: (MonadState Game m, MonadMessage m) => String -> m ()
announce = handleMessage . Announce

queryPlayers :: (MonadState Game m) => (Player -> Bool) -> m [Player]
queryPlayers q = gets (filter q)

isRoleActive :: (MonadState Game m) => Role -> m Bool
isRoleActive role = (not . null) <$> queryPlayers (hasRole role)

forAny :: (MonadState Game m) => Role -> (Player -> m ()) -> m ()
forAny role f = queryPlayers (hasRole role) >>= mapM_ f

nightStart :: MonadWG m => m ()
nightStart = do
  -- Tell any mystics how many werewolf players there are
  forAny Mystic \player -> do
    werewolfPlayers <- queryPlayers $ onTeam WerewolfTeam
    let count = length werewolfPlayers
    pm (name player) $ "There are " ++ show count ++ " players on the werewolf team."

dayStart :: MonadWG m => m ()
dayStart = pure ()

process :: (MonadWG m) => Event -> m ()
process = \case
  DayEnd -> nightStart
  NightEnd -> dayStart
