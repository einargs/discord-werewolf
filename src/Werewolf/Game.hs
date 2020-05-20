module Werewolf.Game
  ( Victory(..)
  , Message(..)
  , Event(..)
  , Action(..)
  , ActionInfo(..)
  , Scope(..)
  , Game(..)
  , Phase(..)
  , Round(..)
  , validPhases
  , playerCan
  , initGame
  ) where

import qualified Data.Text as T
import Optics (view)
import Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels)

import Werewolf.Player

data Victory
  = WerewolfVictory
  | VillagerVictory
  | MonsterVictory
  deriving (Show, Eq)

data Message
  = PM PlayerName T.Text
  | Announce T.Text
  deriving (Show, Eq)

-- | Events other than actions should not have any effects on the
-- state of the game; rather they are used to passively log an occurance.
data Event
  = PlayerAction Action
  | DayEnd
  | NightEnd
  | SendMessage Message
  | PlayerDeath PlayerName
  | Victory Victory
  deriving (Show, Eq)

-- | Whether a command was sent privately, in DMs,
-- or publically in the main chat.
data Scope
  = Public
  | Private
  deriving (Show, Eq, Ord)

data Action = Action
  { playerName :: PlayerName
  , scope :: Scope
  , actionInfo :: ActionInfo
  }
  deriving (Show, Eq)

-- | These correspond directly to commands that players will
-- enter.
data ActionInfo
  = Accuse PlayerName
  | LynchVote Bool
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
  | CupidArrow PlayerName PlayerName
  | ProphetVision Role
  | RevealerKill PlayerName
  | MasonReveal
  | GunnerShoot PlayerName
  | DoppelgangerChoose PlayerName
  | TurncoatSwitch Team
  deriving (Show, Eq)

data Phase = Day | Night
  deriving (Show, Eq)

-- | Gives the phases that an action can be taken during.
validPhases :: Action -> [Phase]
validPhases Action{actionInfo} = case actionInfo of
  Accuse _ -> [Day]
  LynchVote _ -> [Day]
  WerewolfKill _ -> [Day]
  SpellcasterHex _ -> [Night]
  DoctorRevive _ -> [Night]
  SeerClairvoyance _ -> [Night]
  BodyguardProtect _ -> [Night]
  GuardianAngelProtect _ -> [Night]
  HuntressKill _ -> [Night]
  HarlotHideWith _ -> [Night]
  HunterRevenge _ -> [Day, Night]
  MentalistCompare _ _ -> [Night]
  CupidArrow _ _ -> [Night]
  ProphetVision _ -> [Night]
  RevealerKill _ -> [Night]
  MasonReveal -> [Day]
  GunnerShoot _ -> [Day]
  DoppelgangerChoose _ -> [Night]
  TurncoatSwitch _ -> [Night]

-- | Whether the player can or cannot perform an action
-- and why.
data Capability
  = Allowed
  | RoleCannot
  | PlayerIsDead
  | PhaseMustBe Phase
  | CannotBecause T.Text

-- | Determines whether the player can or cannot take
-- an action.
playerCan
  :: Phase -- ^ the current phase (night or day)
  -> Player -- ^ the player taking the action
  -> Action -- ^ the action the player took
  -> Capability
playerCan currentPhase player Action{scope,actionInfo=info} =
  case view #status player of
    Dead -> PlayerIsDead
    Alive -> case (view #roleData player, info) of
      (_, Accuse _) -> Allowed
      (_, LynchVote _) -> Allowed
      (WerewolfData, WerewolfKill _) -> Allowed
      (SpellcasterData hasHexed, SpellcasterHex _) ->
        if hasHexed
          then CannotBecause "The spellcaster can only hex one person per game."
          else Allowed
      (DoctorData, DoctorRevive _) -> Allowed
      (SeerData, SeerClairvoyance _) -> Allowed
      (BodyguardData, BodyguardProtect _) -> Allowed
      (GuardianAngelData lastProtected, GuardianAngelProtect protectee) ->
        case lastProtected of
          Nothing -> Allowed
          Just lastProtectee ->
            if lastProtectee /= protectee
              then Allowed
              else CannotBecause "The guardian angel cannot protect the same person twice in a row."
      (HuntressData hasKilled, HuntressKill _) ->
        if hasKilled
          then CannotBecause "The huntress can only kill one person per game."
          else Allowed
      (HarlotData, HarlotHideWith _) -> Allowed
      (HunterData, HunterRevenge _) -> Allowed
      (MentalistData, MentalistCompare _ _) -> Allowed
      (CupidData hasFired, CupidArrow _ _) ->
        if hasFired
          then CannotBecause "The cupid can only link two people once at the beginning of the game."
          else Allowed
      (ProphetData, ProphetVision _) -> Allowed
      (RevealerData, RevealerKill _) -> Allowed
      (MasonData, MasonReveal) -> Allowed
      (GunnerData c, GunnerShoot _) ->
        if c > 0 then Allowed
                 else CannotBecause "The gunner is out of bullets."
      (DoppelgangerData chosen, DoppelgangerChoose _) ->
        case chosen of
          Nothing -> Allowed
          Just _ -> CannotBecause "The doppelganger can only doppelgang one person per game."
      (TurncoatData _, TurncoatSwitch _) -> Allowed
      _ -> RoleCannot
  where
    requirePhase phase
      | currentPhase == phase = Allowed
      | otherwise = PhaseMustBe phase

newtype Round = Round [Event]
  deriving (Show, Eq)

data Game = Game
  { currentPhase :: Phase
  , players :: [Player]
  , rounds :: [Round]
  } deriving (Show, Eq)

-- | Initialize a game in the first night with the passed
-- list of players.
initGame :: [Player] -> Game
initGame players = Game
  { currentPhase = Night
  , players = players
  , rounds = [Round []]
  }

makeFieldLabelsWith noPrefixFieldLabels ''Game
