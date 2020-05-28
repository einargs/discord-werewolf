module Werewolf.Play.Roles
  ( killPlayer
  , mystic
  ) where

import Control.Monad.Random
import Control.Monad.State.Class

import Optics ((^.), (%))
import Optics.State.Operators ((%=), (.=))

import Data.Text (Text)
import qualified Data.Text as T

import Util.Text (tshow)
import Util.Optics (getv)
import Werewolf.Game
import Werewolf.Player
import qualified Werewolf.Descriptions as Desc
import Werewolf.Play.Base

-- | A common function used by the traitor and doppelganger to assume
-- the role (and modifiers) of a target.
--
-- TODO: may have to change whether the modifiers are copied.
assumeRoleOf
  :: (MonadState Game m)
  => PlayerName -- ^ The player assuming the role
  -> PlayerName -- ^ The player whose role is being copied
  -> m ()
assumeRoleOf copierName copiedName = do
  copied <- getv $ selectPlayer copiedName
  let copierLens = selectPlayer copierName
  copierLens % #modifiers .= copied ^. #modifiers
  copierLens % #roleData .= initialDataFor (playerRole copied)

-- | This function implements the traitors' reactions to a death.
--
-- If there are multiple traitors, this only has one of them copy
-- the death.
traitorReactionsToDeath :: (MonadState Game m) => PlayerName -> m ()
traitorReactionsToDeath deadName = do
  deadPlayer <- getv $ selectPlayer deadName
  when (onTeam WerewolfTeam deadPlayer) $ do
    traitors <- queryPlayers [hasRole Traitor]
    case traitors of
      [] -> pure ()
      traitor:_ -> do
        let traitorName = traitor ^. #name
        traitorName `assumeRoleOf` deadName

-- | This function implements the doppelgangers' reactions to a death.
doppelgangerReactionsToDeath :: (MonadState Game m) => PlayerName -> m ()
doppelgangerReactionsToDeath deadName =
  forAny Doppelganger $ \player ->
    case player ^. #roleData of
      DoppelgangerData (Just target)
        | deadName == target -> do
            (player ^. #name) `assumeRoleOf` target
      _ -> pure ()

-- | This function implements the cupid's reactions to a death.
cupidReactionsToDeath
  :: forall m. (MonadVictory m, MonadCom m, MonadState Game m)
  => PlayerName -> m ()
cupidReactionsToDeath deadName =
  forAny Cupid $ \player ->
    case player ^. #roleData of
      CupidData (Linked p1 p2)
        | p1 == deadName ->
          killIfAlive p2
        | p2 == deadName ->
          killIfAlive p1
      _ -> pure ()
  where
    killIfAlive :: PlayerName -> m ()
    killIfAlive pn = do
      live <- isAlive <$> (getv $ selectPlayer pn)
      when live $ killPlayer pn

-- | This function implements a hunter's revenge mechanic that
-- triggers in reaction to their death.
hunterReactionsToDeath
  :: (MonadVictory m, MonadCom m, MonadState Game m)
  => PlayerName -> m ()
hunterReactionsToDeath deadName = do
  isHunter <- hasRole Hunter <$> getv (selectPlayer deadName)
  when isHunter $ do
    mbTarget <- requestOptionalAction deadName msg $ \Action{actionInfo} ->
      case actionInfo of
        HunterRevenge target -> Just target
        _ -> Nothing
    case mbTarget of
      Nothing -> pure ()
      Just target -> killPlayer target
  where msg = T.concat
          [ "As a hunter, once you have died you can optionally select one person "
          , "to kill. Use `!w pass` to skip this, and use `!w revenge @player "
          , "to choose who to kill."
          ]

-- | This function implements all role-based reactions to a death.
--
-- NOTE: In an ideal world, this would go in Roles.hs, but I don't want
-- to deal with mutually recursive modules right now.
roleReactionsToDeath :: (MonadVictory m, MonadCom m, MonadState Game m) => PlayerName -> m ()
roleReactionsToDeath pn = forM_ reactions $ \reaction -> reaction pn
  where reactions =
          [ hunterReactionsToDeath
          , cupidReactionsToDeath
          , traitorReactionsToDeath
          , doppelgangerReactionsToDeath
          ]

-- | Kill the passed player.
killPlayer
  :: (MonadState Game m, MonadCom m, MonadVictory m)
  => PlayerName
  -> m ()
killPlayer pn = killPlayerWithReaction pn roleReactionsToDeath

-- | Tell any mystics how many werewolf players there are.
mystic :: MonadWG m => m ()
mystic = forAny Mystic $ \player -> do
  werewolfPlayers <- queryPlayers [onTeam WerewolfTeam]
  let countText = tshow $ length werewolfPlayers
  pm (name player) $ T.concat ["There are ", countText, " players on the werewolf team."]

-- | Request that all seers select their clairvoyance target and handles
-- the resulting actions.
seer :: MonadWG m => m ()
seer = forAny Seer $ \player -> do
  target <- requestAction (player ^. #name) requestMsg $ \Action{actionInfo} ->
    case actionInfo of
      SeerClairvoyance target -> Just target
      _ -> Nothing
  targetTeam <- seerTeam <$> getv (selectPlayer target)
  let teamName = Desc.team targetTeam
      msg = T.concat [Desc.mention target, " is on the ", teamName, " team."]
  pm target msg
  where requestMsg = T.concat
          [ "Please select a target to see what team they are on. "
          , "Use `!w see @player` to select the player."
          ]

-- | Request that all mentalists choose who to compare and handle the resulting
-- actions.
mentalist :: MonadWG m => m ()
mentalist = forAny Mentalist $ \player -> do
  let playerName = player ^. #name
  requestAction playerName requestMsg $ \Action{actionInfo} ->
    case actionInfo of
      MentalistCompare p1 p2 -> do
        let teamOf pn = actualTeam <$> getv (selectPlayer pn)
        p1Team <- teamOf p1
        p2Team <- teamOf p2
        pm playerName $ if p1Team == p2Team
          then T.concat [Desc.mention p1, " and ", Desc.mention p2, " are on the same team."]
          else T.concat [Desc.mention p1, " and ", Desc.mention p2, " are not on the same team."]
  where requestMsg = T.concat
          [ "Please select two people to compare which team they are on. "
          , "Use `!w compare @player1 @player2` to choose who to compare."
          ]

-- | Request that any prophets choose which role to recieve information about.
prophet :: MonadWG m => m ()
prophet = forAny Prophet $ \player -> do
  let name = player ^. #name
  role <- requestAction playerName requestMsg $ \Action{actionInfo} ->
    case actionInfo of
      ProphetVision r -> Just r
      _ -> Nothing
  isRoleActive role >>= \isActive ->
    pm playerName $ case isActive of
      True -> T.concat ["There are active ", Desc.pluralRole role, "."]
      False -> T.concat ["No ", Desc.pluralRole role, " are active."]
  where requestMsg = T.concat
          [ "Please select a role to find out whether it is active or not. "
          , "Use `!w vision <role>` to select the role."
          ]

-- |
turncoatFirstNight :: MonadWG m => m ()
turncoatFirstNight = forAny Turncoat $ \player -> do
  let playerName = player ^. #name
  team <- requestAction playerName requestMsg $ \Action{actionInfo} ->
    case actionInfo of
      MentalistCompare p1 p2 -> Just (p1, p2)
      _ -> Nothing
  roundNum <- roundCount
  selectPlayer playerName % #roleData .= TurncoatData team (Just roundNum)
  where requestMsg = T.concat
          [ "Please select a role to find out whether it is active or not. "
          , "Use `!w vision <role>` to select the role."
          ]

-- |
turncoatNightly :: MonadWG m => m ()
turncoatNightly = forAny Turncoat $ \Player{name, roleData} -> do
  roundNum <- roundCount
  let count = case roleData of
    TurncoatData _ (Just count) -> count
    _ -> error "should have turncoat data with a count."
  -- The turncoat can only go once every other round.
  when (count + 1 < roundNum)

-- |
firstNightActions :: MonadWG m => m ()
firstNightActions = do
  -- The turncoat goes first so that if the seer chooses them, the seer
  -- will see the role they've chosen. This also means that the mystic's
  -- werewolf team count will include the turncoat if they've selected
  -- the werewolves.
  turncoatFirstNight
  mystic
  seer

-- |
nightlyActions :: MonadWG m => m ()
nightlyActions = do
  -- The turncoat goes first because they go first on the first night.
  turncoatNightly
  mystic
  seer
