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
  , TimerName(..)
  , Capability(..)
  , CapabilityError(..)
  , playerCan
  , initGame
  ) where

import qualified Data.Text as T
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as Seq
import Optics (view, (^.))
import Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels)
import Data.Maybe (isNothing)

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
  | NightStart
  -- ^ Only the transition from day to night in the middle of a round matters;
  -- the start of a day and end of a night can be infered from the round begining
  -- and ending.
  | SendMessage Message
  | SuccessfullyLynched PlayerName
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
  | SecondAccusation PlayerName -- ^ The player you are seconding
  | LynchVote Bool
  | WerewolfKill PlayerName
  | SpellcasterHex PlayerName
  | WarlockCurse PlayerName
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
  | Pass -- ^ pass on performing an optional action at night.
  deriving (Show, Eq)

data Phase = Day | Night
  deriving (Show, Eq)

-- | Reasons why a player wouldn't be able to perform
-- an action.
data CapabilityError
  = RoleCannot
  | StatusMustBe PlayerStatus
  | PhaseMustBe Phase
  | ScopeMustBe Scope
  | OtherPlayerStatusMustBe PlayerName PlayerStatus
  | PassInvalid
  | OtherReason T.Text

-- | Whether the player can or cannot perform an action
-- and why.
data Capability
  = Allowed
  | CannotBecause CapabilityError

instance Semigroup Capability where
  Allowed <> Allowed = Allowed
  Allowed <> e = e
  e <> _ = e

instance Monoid Capability where
  mempty = Allowed

-- | Determines whether the player can or cannot take
-- an action.
playerCan
  :: Phase -- ^ The current phase (night or day)
  -> Bool -- ^ Whether a pass would be acceptable in the current context
  -> Int -- ^ The current round number
  -> (PlayerName -> PlayerStatus)
  -- ^ A function that gets the status of any player in the game.
  -> Player -- ^ The player taking the action
  -> Action -- ^ The action the player took
  -> Capability
playerCan
  currentPhase
  isPassValid
  roundNumber
  otherPlayerStatus
  player
  Action{scope,actionInfo=info} =
    case (player ^. #roleData, info) of
      (_, Pass) -> if isPassValid then Allowed else CannotBecause PassInvalid
      (_, Accuse target) -> mconcat
        [ requireStd Day Public
        , requireLivePlayer target
        , requireNotSelf target "You cannot accuse yourself."
        ]
      (_, LynchVote _) -> requireStd Day Private
      (WerewolfData, WerewolfKill target) -> requireStd Night Private
        <> requireLivePlayer target
        <> requireNotSelf target "The werewolf cannot kill themselves."
      (WarlockData hasCursed, WarlockCurse target) -> mconcat
        [ requireAlive
        , requireScope Private
        , requireLivePlayer target
        , require (not hasCursed) "The warlock can only curse one person per game."
        ]
      (SpellcasterData hasHexed, SpellcasterHex target) -> mconcat
        [ requireAlive
        , requireScope Private
        , requireLivePlayer target
        , require (not hasHexed) "The spellcaster can only hex one person per game."
        ]
      (DoctorData hasRevivedSelf, DoctorRevive target) -> mconcat
        [ requirePhase Night
        , requireScope Private
        , require (not hasRevivedSelf) "After reviving themselves the doctor can no longer revive anyone."
        -- Require that whoever the target is be dead.
        , if player ^. #name == target
            then requireStatus Dead
            else requirePlayerStatus target Dead
        ]
      (SeerData, SeerClairvoyance target) -> mconcat
        [ requireStd Night Private
        , requireLivePlayer target
        ]
      (BodyguardData, BodyguardProtect target) -> mconcat
        [ requireStd Night Private
        , requireLivePlayer target
        ]
      (GuardianAngelData lastProtected, GuardianAngelProtect protectee) -> mconcat
        [ requireStd Night Private
        , requireLivePlayer protectee
        , case lastProtected of
            Nothing -> Allowed
            Just lastProtectee ->
              require (lastProtectee /= protectee)
                "The guardian angel cannot protect the same person twice in a row."
        ]
      (HuntressData hasKilled, HuntressKill target) -> mconcat
        [ requireStd Night Private
        , requireLivePlayer target
        , requireNotSelf target "The huntress cannot kill themselves."
        , require (not hasKilled) "The huntress can only kill one person per game."
        ]
      (HarlotData, HarlotHideWith target) -> mconcat
        [ requireStd Night Private
        , requireLivePlayer target
        ]
      (HunterData hasTakenRevenge, HunterRevenge target) -> mconcat
        [ requireStatus Dead
        , requireScope Public
        -- This has to go first, because the hunter will always be dead
        -- by this point; if it came after the live target check it
        -- would never occur.
        , requireNotSelf target "The hunter cannot take revenge against themselves"
        , requireLivePlayer target
        , require (not hasTakenRevenge)
          "The hunter can only take revenge against one person after dying."
        ]
      (MentalistData, MentalistCompare n1 n2) -> mconcat
        [ requireStd Night Private
        , requireLivePlayer n1
        , requireLivePlayer n2
        , require (n1 /= n2) "The seer cannot compare a person to themselves."
        , let pn = player ^. #name
              cond = pn /= n1 && pn /= n2
              in require cond "The mentalist cannot compare themselves to anyone else."
        ]
      (CupidData linkStatus, CupidArrow n1 n2) -> mconcat
        [ requireStd Night Private
        , requireLivePlayer n1
        , requireLivePlayer n2
        , requireNotSelf n1 "The cupid cannot link themselves."
        , requireNotSelf n2 "The cupid cannot link themselves."
        , require (n1 /= n2) "The cupid cannot link the same person."
        , require (linkStatus == NoLink)
          "The cupid can only link two people once at the beginning of the game."
        ]
      (ProphetData, ProphetVision _) -> requireStd Night Private
      (RevealerData, RevealerKill target) -> requireStd Night Private
        <> requireLivePlayer target
        <> requireNotSelf target "The revealer cannot target themselves."
      (MasonData, MasonReveal) -> requireStd Day Public
      (GunnerData c, GunnerShoot target) -> mconcat
        [ requireAlive
        , requireScope $ case currentPhase of
            Day -> Public
            Night -> Private
        , requireLivePlayer target
        , requireNotSelf target "The gunner cannot shoot themselves."
        , require (c > 0) "The gunner is out of bullets."
        ]
      (DoppelgangerData chosen, DoppelgangerChoose _) -> mconcat
        [ requireStd Night Private
        , case chosen of
            Nothing -> Allowed
            Just _ -> CannotBecause $ OtherReason
              "The doppelganger can only doppelgang one person per game."
        ]
      (TurncoatData _ lastSwitched, TurncoatSwitch _) -> mconcat
        [ requireStd Night Private
        , case lastSwitched of
            Nothing -> Allowed
            Just n -> require (n < roundNumber - 1)
              "The turncoat cannot switch sides twice in a row."
        ]
      _ -> CannotBecause RoleCannot
  where
    -- | A default set of requirements. Requires that the scope and
    -- phase be the passed arguments, as well as that the player be
    -- alive.
    requireStd p s = requireAlive <> requirePhase p <> requireScope s
    -- | Require that the current phase be `phase`.
    requirePhase phase
      | currentPhase == phase = Allowed
      | otherwise = CannotBecause $ PhaseMustBe phase
    -- | Require that the boolean argument be true, or
    -- fail with a textual explanation.
    require True _ = Allowed
    require False reason = CannotBecause $ OtherReason reason
    -- | Require that the current scope be `scope`.
    requireScope s
      | s == scope = Allowed
      | otherwise = CannotBecause $ ScopeMustBe s
    -- | Require that the player be alive.
    requireAlive = requireStatus Alive
    -- | Require that the player's current status be `status`.
    requireStatus status
      | status == view #status player = Allowed
      | otherwise = CannotBecause $ StatusMustBe status
    -- | Require that the passed player name not be the same as the player
    -- taking the action.
    requireNotSelf name reason
      | name /= view #name player = Allowed
      | otherwise = CannotBecause $ OtherReason reason
    -- | Utility for `requirePlayerStatus` that just requires the other
    -- player to be alive.
    requireLivePlayer name = requirePlayerStatus name Alive
    -- | Require that the status of the other player be `status`.
    requirePlayerStatus name status
      | otherPlayerStatus name == status = Allowed
      | otherwise = CannotBecause $ OtherPlayerStatusMustBe name status

newtype Round = Round [Event]
  deriving (Show, Eq)

data TimerName
  = AccusationsAllowedTimer
  | NightEndTimer
  | SecondingAccusationTimer
  deriving (Show, Eq, Ord)

data Game = Game
  { currentPhase :: Phase
  , players :: [Player]
  , rounds :: [Round]
  , actionBuffer :: Seq Action
  , timerBuffer :: Seq TimerName
  } deriving (Show, Eq)
makeFieldLabelsWith noPrefixFieldLabels ''Game

-- | Initialize a game in the first night with the passed
-- list of players.
initGame :: [Player] -> Game
initGame players = Game
  { currentPhase = Night
  , players = players
  , rounds = [Round []]
  , actionBuffer = Seq.empty
  }
