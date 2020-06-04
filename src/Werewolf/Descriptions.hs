-- | This module is intended to be imported qualified.
--
-- This module contains functions for converting various data
-- types into human readable phrases.
module Werewolf.Descriptions
  ( mention
  , team
  , status
  , phase
  , capabilityError
  , role
  , pluralRole
  ) where

import qualified Data.Text as T
import Data.Text (Text)

import Util.Text (tshow)

import Werewolf.Player hiding (status)
import Werewolf.Game

-- | Convert a player name into a mention of that player.
mention :: PlayerName -> Text
mention (PlayerName uid) = T.concat ["<@!", tshow uid, ">"]

-- | Describes the team.
team :: Team -> Text
team = \case
  WerewolfTeam -> "werewolf"
  VillagerTeam -> "villager"
  NeutralTeam -> "neutral"

-- | Describes the status.
status :: PlayerStatus -> Text
status = \case
  Alive -> "alive"
  Dead -> "dead"

-- | Describes the phase.
phase :: Phase -> Text
phase = \case
  Day -> "day"
  Night -> "night"

-- | This converts a capability error into the error message that
-- will be presented to the player.
capabilityError :: CapabilityError -> Text
capabilityError = \case
  RoleCannot -> "Your role cannot perform that action."
  StatusMustBe s -> T.concat ["You must be ", status s, " to perform that action."]
  PhaseMustBe p -> T.concat ["It must be ", phase p, " to perform that action."]
  ScopeMustBe Public -> "That command must be sent in the public channel."
  ScopeMustBe Private -> "That command must be sent as a DM to the bot."
  OtherPlayerStatusMustBe pn s -> T.concat [mention pn, " must be ", status s]
  PassInvalid -> "There is nothing to skip using `!w pass`."
  OtherReason txt -> txt

-- | Describe the role; provides singular and plural versions.
roleBase :: Role -> (Text, Text)
roleBase = \case
    Werewolf -> ("werewolf", "werewolves")
    Werecub -> addS "werecub"
    Werekitten -> addS "werekitten"
    Spellcaster -> addS "spellcaster"
    ToughWolf -> ("tough wolf", "tough wolves")
    Traitor -> addS "traitor"
    Warlock -> addS "warlock"
    Doctor -> addS "doctor"
    Seer -> addS "seer"
    Bodyguard -> addS "bodyguard"
    GuardianAngel -> addS "guardian angel"
    Huntress -> ("huntress", "huntresses")
    Harlot -> addS "harlot"
    Hunter -> addS "hunter"
    Mentalist -> addS "mentalist"
    MadScientist -> addS "mad scientist"
    Cupid -> addS "cupid"
    Mystic -> addS "mystic"
    Prophet -> addS "prophet"
    Revealer -> addS "revealer"
    Lycan -> ("lycan", "lycan")
    Mason -> addS "mason"
    Gunner -> addS "gunner"
    Prince -> addS "prince"
    Doppelganger -> addS "doppelganger"
    Monster -> addS "monster"
    Turncoat -> addS "turncoat"
    Villager -> addS "villager"
  where
    addS txt = (txt, T.snoc txt 's')

-- | Get a singular word describing a role.
role :: Role -> Text
role = fst . roleBase

-- | Get a plural word describing a role.
pluralRole :: Role -> Text
pluralRole = snd . roleBase
