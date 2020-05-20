module Command
  ( Command(..)
  , CommandError(..)
  , Scope(..)
  , requireScope
  ) where

import Discord.Types (UserId)
import Werewolf.Game (ActionInfo, Scope(..), Action)
import Werewolf.Player (PlayerName(..))

data CommandError
  = UserNotPlaying UserId
  | UnrecognizedCommand
  | ScopeShouldBe Scope
  deriving (Show, Eq, Ord)

data Command
  = WerewolfAction ActionInfo
  | AddPlayer UserId
  | RemovePlayer PlayerName
  | Join
  | Leave
  | StartGame
  deriving (Show, Eq)

isScopeValid :: Scope -> Command -> Bool
isScopeValid scope = \case
  WerewolfAction _info -> True
  AddPlayer _ -> scope == Public
  RemovePlayer _ -> scope == Public
  Join -> scope == Public
  Leave -> scope == Public
  StartGame -> scope == Public

requireScope
  :: Scope
  -> Command
  -> Either CommandError Command
requireScope scope c = if isScopeValid scope c
  then Right c
  else Left $ ScopeShouldBe scope
