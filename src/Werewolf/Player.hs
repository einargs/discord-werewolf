module Werewolf.Player
  ( playerTeams
  , actualTeam
  , seerTeam
  , teamsForRole
  , playerRole
  , hasRole
  , onTeam
  , allRoles
  , initialDataFor
  , roleGroup
  , Player(..)
  , PlayerName(..)
  , Role(..)
  , RoleData(..)
  , Modifier(..)
  , Team(..)
  , PlayerStatus(..)
  , LycanStatus(..)
  , RoleGroup(..)
  ) where

import Discord.Types (UserId)
import Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels)

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
  deriving (Show, Eq, Enum, Bounded)

allRoles :: [Role]
allRoles = [minBound..maxBound]

data LycanStatus
  = Turned
  | Unturned
  deriving (Show, Eq)

data RoleData
  = WerewolfData
  | WerecubData
  | WerekittenData
  | SpellcasterData Bool -- ^ Has hexed someone
  | ToughWolfData Bool -- ^ Has been lynched once
  | TraitorData
  | WarlockData Bool -- ^ Has cursed someone
  | DoctorData
  | SeerData
  | BodyguardData
  | GuardianAngelData (Maybe PlayerName) -- ^ last guarded
  | HuntressData Bool -- ^ Has the huntress killed someone
  | HarlotData
  | HunterData Bool -- ^ Has taken revenge
  | MentalistData
  | MadScientistData
  | CupidData Bool -- ^ Has the cupid linked two people
  | MysticData
  | ProphetData
  | RevealerData
  | LycanData LycanStatus -- ^ Has the lycan been turned
  | MasonData
  | GunnerData Int -- ^ Remaining bullets
  | PrinceData
  | DoppelgangerData (Maybe PlayerName) -- ^ doppelganger target
  | MonsterData
  | TurncoatData
    Team -- ^ The team the turncoat is on; starts as neutral.
    (Maybe Int) -- ^ The last round the turncoat switched teams on.
  | VillagerData
  deriving (Show, Eq)

initialDataFor :: Role -> RoleData
initialDataFor = \case
  Werewolf -> WerewolfData
  Werecub -> WerecubData
  Werekitten -> WerekittenData
  Spellcaster -> SpellcasterData False
  ToughWolf -> ToughWolfData False
  Traitor -> TraitorData
  Warlock -> WarlockData False
  Doctor -> DoctorData
  Seer -> SeerData
  Bodyguard -> BodyguardData
  GuardianAngel -> GuardianAngelData Nothing
  Huntress -> HuntressData False
  Harlot -> HarlotData
  Hunter -> HunterData False
  Mentalist -> MentalistData
  MadScientist -> MadScientistData
  Cupid -> CupidData False
  Mystic -> MysticData
  Prophet -> ProphetData
  Revealer -> RevealerData
  Lycan -> LycanData Unturned
  Mason -> MasonData
  Gunner -> GunnerData 2
  Prince -> PrinceData
  Doppelganger -> DoppelgangerData Nothing
  Monster -> MonsterData
  Turncoat -> TurncoatData NeutralTeam Nothing
  Villager -> VillagerData

roleForData :: RoleData -> Role
roleForData = \case
  WerewolfData -> Werewolf
  WerecubData -> Werecub
  WerekittenData -> Werekitten
  SpellcasterData _ -> Spellcaster
  ToughWolfData _ -> ToughWolf
  TraitorData -> Traitor
  WarlockData _ -> Warlock
  DoctorData -> Doctor
  SeerData -> Seer
  BodyguardData -> Bodyguard
  GuardianAngelData _ -> GuardianAngel
  HuntressData _ -> Huntress
  HarlotData -> Harlot
  HunterData _ -> Hunter
  MentalistData -> Mentalist
  MadScientistData -> MadScientist
  CupidData _ -> Cupid
  MysticData -> Mystic
  ProphetData -> Prophet
  RevealerData -> Revealer
  LycanData _ -> Lycan
  MasonData -> Mason
  GunnerData _ -> Gunner
  PrinceData -> Prince
  DoppelgangerData _ -> Doppelganger
  MonsterData -> Monster
  TurncoatData _ _ -> Turncoat
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
--
-- This function is primarily intended to be
-- used for help commands; use teamsForRoleData
-- to actually get the correct team based on
-- role data.
teamsForRole :: Role -> (Team, Team)
teamsForRole = \case
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
  where
    w = WerewolfTeam
    v = VillagerTeam
    n = NeutralTeam

-- | Returns a tuple of the role's actual
-- team and the team that the seer sees
-- them as being on.
--
-- This takes into account information from
-- the role data (e.g. whether the lycan has
-- been turned).
teamsForRoleData :: RoleData -> (Team, Team)
teamsForRoleData = \case
  WerewolfData -> (w, w)
  WerecubData -> (w, w)
  WerekittenData -> (w, v)
  SpellcasterData _ -> (w, v)
  ToughWolfData _ -> (w, w)
  TraitorData -> (w, v)
  WarlockData _ -> (w, v)
  DoctorData -> (v, v)
  SeerData -> (v, v)
  BodyguardData -> (v, v)
  GuardianAngelData _ -> (v, v)
  HuntressData _ -> (v, v)
  HarlotData -> (v, v)
  HunterData _ -> (v, v)
  MentalistData -> (v, v)
  MadScientistData -> (v, w)
  CupidData _ -> (v, v)
  MysticData -> (v, v)
  ProphetData -> (v, v)
  RevealerData -> (v, v)
  LycanData Turned -> (w, w)
  LycanData Unturned -> (v, w)
  MasonData -> (v, v)
  GunnerData _ -> (v, v)
  PrinceData -> (v, v)
  DoppelgangerData _ -> (n, v)
  MonsterData -> (n, w)
  TurncoatData _ _ -> (n, n)
  VillagerData -> (v, v)
  where
    w = WerewolfTeam
    v = VillagerTeam
    n = NeutralTeam

-- | A data type listing the different groups
-- that a role can be in. In each game, there
-- can only be one role from a given group.
data RoleGroup
  = Assassins
  | Guardians
  | Chaos
  | Information

rolesInGroup :: RoleGroup -> [Role]
rolesInGroup = \case
  Assassins -> [Huntress, Hunter, Gunner, Revealer]
  Guardians -> [GuardianAngel, Bodyguard, Doctor]
  Chaos -> [Cupid, MadScientist]
  Information -> [Seer, Mystic, Prophet, Mentalist]

roleGroup :: Role -> Maybe RoleGroup
roleGroup = \case
  -- Assassins
  Huntress -> Just Assassins
  Hunter -> Just Assassins
  Gunner -> Just Assassins
  Revealer -> Just Assassins
  -- Guardians
  GuardianAngel -> Just Guardians
  Bodyguard -> Just Guardians
  Doctor -> Just Guardians
  -- Chaos
  Cupid -> Just Chaos
  MadScientist -> Just Chaos
  -- Information
  Seer -> Just Information
  Mystic -> Just Information
  Prophet -> Just Information
  Mentalist -> Just Information
  -- Any others have no role group
  _ -> Nothing

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

makeFieldLabelsWith noPrefixFieldLabels ''Player

playerRole :: Player -> Role
playerRole = roleForData . roleData

-- | Returns the player's actual team
-- and the team they're seen as by the
-- seer.
playerTeams :: Player -> (Team, Team)
playerTeams Player {roleData, modifiers} =
  if Minion `elem` modifiers
    then (WerewolfTeam, WerewolfTeam)
    else teamsForRoleData roleData

-- | Returns the player's actual team.
actualTeam :: Player -> Team
actualTeam = fst . playerTeams

-- | Returns the team the player is seen as being
-- on by the seer.
seerTeam :: Player -> Team
seerTeam = snd . playerTeams

hasRole :: Role -> Player -> Bool
hasRole role Player{roleData} = role == roleForData roleData

onTeam :: Team -> Player -> Bool
onTeam team = (team==) . actualTeam
