module Werewolf.Player
  ( playerTeams
  , actualTeam
  , seerTeam
  , hasRole
  , playerRole
  , Player(name, roleData, modifiers, status)
  , PlayerName
  , Role
  , Modifier
  , Team
  ) where

import Discord.Types (UserId)

data Role
  = Werewolf
  | Werecub
  | Werekitten
  | Spellcaster
  | ToughWolf
  | Traitor
  | Warlock
  | Doctor
  | Seer
  | Bodyguard
  | GuardianAngel
  | Huntress
  | Harlot
  | Hunter
  | Mentalist
  | MadScientist
  | Cupid
  | Mystic
  | Prophet
  | Revealer
  | Lycan
  | Mason
  | Gunner
  | Prince
  | Doppelganger
  | Monster
  | Turncoat
  | Villager
  deriving (Show, Eq, Enum)

data RoleData
  = WerewolfData
  | WerecubData
  | WerekittenData
  | SpellcasterData Bool -- ^ Has hexed someone
  | ToughWolfData Bool -- ^ Has been lynched once
  | TraitorData
  | WarlockData
  | DoctorData
  | SeerData
  | BodyguardData
  | GuardianAngelData PlayerName -- ^ last guarded
  | HuntressData Bool -- ^ Has the huntress killed someone
  | HarlotData
  | HunterData
  | MentalistData
  | MadScientistData
  | CupidData
  | MysticData
  | ProphetData
  | RevealerData
  | LycanData Bool -- ^ Has the lycan been turned
  | MasonData
  | GunnerData Int -- ^ Remaining bullets
  | PrinceData
  | DoppelgangerData (Maybe PlayerName) -- ^ doppelganger target
  | MonsterData
  | TurncoatData
  | VillagerData
  deriving (Show, Eq)

roleForData :: RoleData -> Role
roleForData = \case
  WerewolfData -> Werewolf
  WerecubData -> Werecub
  WerekittenData -> Werekitten
  SpellcasterData _ -> Spellcaster
  ToughWolfData _ -> ToughWolf
  TraitorData -> Traitor
  WarlockData -> Warlock
  DoctorData -> Doctor
  SeerData -> Seer
  BodyguardData -> Bodyguard
  GuardianAngelData _ -> GuardianAngel
  HuntressData _ -> Huntress
  HarlotData -> Harlot
  HunterData -> Hunter
  MentalistData -> Mentalist
  MadScientistData -> MadScientist
  CupidData -> Cupid
  MysticData -> Mystic
  ProphetData -> Prophet
  RevealerData -> Revealer
  LycanData _ -> Lycan
  MasonData -> Mason
  GunnerData _ -> Gunner
  PrinceData -> Prince
  DoppelgangerData _ -> Doppelganger
  MonsterData -> Monster
  TurncoatData -> Turncoat
  VillagerData -> Villager

data Modifier
  = Drunk
  | Minion
  deriving (Show, Eq)

data Team
  = WerewolfTeam
  | VillagerTeam
  | NeutralTeam
  deriving (Show, Eq)

-- | Returns a tuple of the role's actual
-- team and the team that the seer sees
-- them as being on.
roleTeams :: Role -> (Team, Team)
roleTeams = let
  w = WerewolfTeam
  v = VillagerTeam
  n = NeutralTeam
  in \case
  Werewolf -> (w, w)
  Werecub -> (w, w)
  Werekitten -> (w, v)
  Spellcaster -> (w, v)
  ToughWolf -> (w, w)
  Traitor -> (w, v)
  Warlock -> (w, v)
  Doctor -> (v, v)
  Seer -> (v, v)
  Bodyguard -> (v, v)
  GuardianAngel -> (v, v)
  Huntress -> (v, v)
  Harlot -> (v, v)
  Hunter -> (v, v)
  Mentalist -> (v, v)
  MadScientist -> (v, w)
  Cupid -> (v, v)
  Mystic -> (v, v)
  Prophet -> (v, v)
  Revealer -> (v, v)
  Lycan -> (v, w)
  Mason -> (v, v)
  Gunner -> (v, v)
  Prince -> (v, v)
  Doppelganger -> (n, v)
  Monster -> (n, w)
  Turncoat -> (n, n)
  Villager -> (v, v)

newtype PlayerName = PlayerName UserId
  deriving (Show, Eq, Ord)

data PlayerStatus = Alive | Dead
  deriving (Show, Eq)

data Player = Player
  { name :: PlayerName
  , roleData :: RoleData
  , modifiers :: [Modifier]
  , status :: PlayerStatus
  } deriving (Show, Eq)

playerRole :: Player -> Role
playerRole = roleForData . roleData

-- | Returns the player's actual team
-- and the team they're seen as by the
-- seer.
playerTeams :: Player -> (Team, Team)
playerTeams Player {roleData, modifiers} =
  if Minion `elem` modifiers
    then (WerewolfTeam, WerewolfTeam)
    else roleTeams $ roleForData roleData

-- | Returns the player's actual team.
actualTeam :: Player -> Team
actualTeam = fst . playerTeams

-- | Returns the team the player is seen as being
-- on by the seer.
seerTeam :: Player -> Team
seerTeam = snd . playerTeams

hasRole :: Role -> Player -> Bool
hasRole role Player{roleData} = role == (roleForData roleData)
