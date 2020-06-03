module Werewolf.Play.Roles
  ( killPlayer
  , removeHexed
  , nightlyActions
  , firstNightActions
  ) where

import Control.Monad.Random
import Control.Monad.State.Class
import Control.Exception (assert)

import Optics ((^.), (%), view, mapping)
import Optics.State.Operators ((%=), (.=))

import Data.Text (Text)
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
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
-- Has a built in safety that prevents you from ending up with
-- multiple werewolves. If there is already one (live) werewolf
-- in the game, and the copying would cause there to be two, this
-- fails.
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
  -- If the copied player is a werewolf, and there is another
  -- (live) werewolf in the game, this will skip.
  werewolfActive <- isRoleActive Werewolf
  let shouldSkip = werewolfActive && playerRole copied == Werewolf
  when shouldSkip $ do
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
    traitors <- queryPlayers [hasRole Traitor, notHexed]
    case traitors of
      [] -> pure ()
      traitor:_ -> do
        let traitorName = traitor ^. #name
        traitorName `assumeRoleOf` deadName

-- | This function implements the werecub's ability to take over from
-- a werewolf that dies.
--
-- If there are multiple werecubs, this only has one of them copy
-- the werewolf.
werecubReactionsToDeath :: (MonadState Game m) => PlayerName -> m ()
werecubReactionsToDeath deadName = do
  role <- playerRole <$> getv (selectPlayer deadName)
  when (role == Werewolf) $ do
    werecubs <- queryPlayers [hasRole Werecub, notHexed]
    case werecubs of
      [] -> pure ()
      werecub:_ ->
        (werecub ^. #name) `assumeRoleOf` deadName

-- | This function implements the doppelgangers' reactions to a death.
doppelgangerReactionsToDeath :: (MonadState Game m) => PlayerName -> m ()
doppelgangerReactionsToDeath deadName =
  forAny Doppelganger $ \player ->
    when (notHexed player) $
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
    when (notHexed player) $
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
  deadPlayer <- getv (selectPlayer deadName)
  when (hasRole Hunter deadPlayer && notHexed deadPlayer) $ do
    mbTarget <- requestOptionalActionWithMessage deadName msg $ \Action{actionInfo} ->
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

-- | A data type that represents an action taken at night.
data NightAction (m :: Type -> Type) = NightAction
  { actionPrompt :: Maybe Text
  , predicate :: Player -> Bool
  , nightAction :: Player -> m ()
  }

-- | Build the predicate for the `silentAction` and `promptedAction`
-- conveinences.
nightActionPredicate :: Role -> Player -> Bool
nightActionPredicate role player = hasRole role player && notHexed player

-- | Convenience for building a NightAction without a prompt.
silentAction :: Role -> (Player -> m ()) -> NightAction m
silentAction role action = NightAction Nothing (nightActionPredicate role) action

-- | Convenience for building a NightAction with a prompt message.
promptedAction :: Text -> Role -> (Player -> m ()) -> NightAction m
promptedAction msg role action = NightAction (Just msg) (nightActionPredicate role) action

-- | Tell any mystics how many werewolf players there are.
mystic :: MonadWG m => NightAction m
mystic = silentAction Mystic $ \player -> do
  werewolfPlayers <- queryPlayers [onTeam WerewolfTeam]
  let countText = tshow $ length werewolfPlayers
  pm (name player) $ T.concat ["There are ", countText, " players on the werewolf team."]

-- | Request that all seers select their clairvoyance target and handles
-- the resulting actions.
seer :: MonadWG m => NightAction m
seer = promptedAction requestMsg Seer $ \player -> do
  target <- requestAction (player ^. #name) $ \Action{actionInfo} ->
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
mentalist :: MonadWG m => NightAction m
mentalist = promptedAction requestMsg Mentalist $ \player -> do
  let playerName = player ^. #name
  (p1, p2) <- requestAction playerName $ \Action{actionInfo} ->
    case actionInfo of
      MentalistCompare p1 p2 -> Just (p1, p2)
      _ -> Nothing
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
prophet :: MonadWG m => NightAction m
prophet = promptedAction requestMsg Prophet $ \player -> do
  let name = player ^. #name
  role <- requestAction name $ \Action{actionInfo} ->
    case actionInfo of
      ProphetVision r -> Just r
      _ -> Nothing
  isRoleActive role >>= \isActive ->
    pm name $ case isActive of
      True -> T.concat ["There are active ", Desc.pluralRole role, "."]
      False -> T.concat ["No ", Desc.pluralRole role, " are active."]
  where requestMsg = T.concat
          [ "Please select a role to find out whether it is active or not. "
          , "Use `!w vision <role>` to select the role."
          ]

-- | This is a helper utility for setting a turncoat's team.
--
-- Preconditions: at least one round must have passed since the
-- turncoat last set their team and the player must be a turncoat.
setTurncoatTeam :: MonadWG m => PlayerName -> Team -> m ()
setTurncoatTeam name team = do
  roleData <- getv roleLens
  roundNum <- roundCount

  -- This checks all of the preconditions.
  case roleData of
    TurncoatData _ (Just count) ->
      assert (roundNum + 1 > count) $ pure ()
    TurncoatData _ Nothing -> pure ()
    _ -> error "Attempted to use setTurncoatTeam on a player who isn't a Turncoat"

  roleLens .= TurncoatData team (Just roundNum)
  where roleLens = selectPlayer name % #roleData

-- |
turncoatFirstNight :: MonadWG m => NightAction m
turncoatFirstNight = promptedAction requestMsg Turncoat $ \player -> do
  let playerName = player ^. #name
  team <- requestAction playerName $ \Action{actionInfo} ->
    case actionInfo of
      TurncoatSwitch team -> Just team
      _ -> Nothing
  setTurncoatTeam playerName team
  where requestMsg =
          "Please select a team to play on. Use `!w switch <team>` to select the team."

-- |
turncoatNightly :: MonadWG m => NightAction m
turncoatNightly = promptedAction requestMsg Turncoat $ \Player{name, roleData} -> do
  roundNum <- roundCount
  let count =
        case roleData of
          TurncoatData _ (Just count) -> count
          _ -> error "should have turncoat data with a count."
  -- The turncoat can only go once every other round.
  when (count + 1 < roundNum) $ do
    mbTeam <- requestOptionalAction name $ \Action{actionInfo} ->
      case actionInfo of
        TurncoatSwitch team -> Just team
        _ -> Nothing
    case mbTeam of
      Just team -> setTurncoatTeam name team
      Nothing -> pure ()
  where requestMsg = T.concat
          [ "Please select a team to play on. "
          , "Use `!w switch <team>` to select the team. "
          , "Use `!w pass` to skip changing your team if you want."
          ]

-- | The werewolf actions.
werewolf :: MonadWG m => NightAction m
werewolf = promptedAction requestMsg Werewolf $ \Player{name, roleData} -> do
  mbTarget <- requestOptionalAction name getWerewolfKill
  kill mbTarget
  killTwice <- didWerecubDie
  when killTwice $ do
    mbSecondTarget <- requestOptionalActionWithMessage
      name secondKillMsg getWerewolfKill
    kill mbSecondTarget
  where
    getWerewolfKill Action{actionInfo} = case actionInfo of
      WerewolfKill target -> Just target
      _ -> Nothing
    kill = \case
      Nothing -> pure ()
      Just target -> do
        let lens = selectPlayer target % #roleData
        roleData <- getv lens
        case roleData of
          LycanData Unturned -> lens .= LycanData Turned
          _ -> killPlayer target
    didWerecubDie = do
      mbLynchedPlayer <- searchCurrentRound $ \case
        SuccessfullyLynched n -> Just n
        _ -> Nothing
      case mbLynchedPlayer of
        Nothing -> pure False
        Just lynchedPlayer -> do
          role <- playerRole <$> getv (selectPlayer lynchedPlayer)
          pure $ role == Werecub
    requestMsg = T.concat
      [ "Choose who to kill or pass on killing this round. "
      , "Use `!w kill @player` to kill someone. "
      , "Use `!w pass` to skip killing someone."
      ]
    secondKillMsg = T.concat
      [ "The werecub died during this turn, so you may kill a second time. "
      , "Use `!w kill @player` to kill someone. "
      , "Use `!w pass` to skip killing someone."
      ]

-- | Handle the revealer's night actions.
revealer :: MonadWG m => NightAction m
revealer = promptedAction requestMsg Revealer $ \Player{name} -> do
  mbTarget <- requestOptionalAction name $ \Action{actionInfo} ->
    case actionInfo of
      RevealerKill target -> Just target
      _ -> Nothing
  case mbTarget of
    Nothing -> pure ()
    Just target -> do
      role <- playerRole <$> getv (selectPlayer target)
      case role of
        Werewolf -> killPlayer target
        Monster -> killPlayer target
        _ -> killPlayer name
  where requestMsg = T.concat
          [ "As the revealer, you can choose to target one person per night. "
          , "If that person is the werewolf or the monster, they die; otherwise, "
          , "you die. Use `!w reveal @player` to choose who to reveal or `!w pass` "
          , "to skip."
          ]

-- | Tell the minion who is on the werewolf team.
minionFirstNight :: MonadWG m => NightAction m
minionFirstNight = NightAction Nothing (hasModifier Minion) $ \Player{name} -> do
  werewolfPlayers <- view (mapping #name) <$> queryPlayers [onTeam WerewolfTeam]
  let nameText = T.intercalate ", " $ Desc.mention <$> werewolfPlayers
      msg = T.concat ["The werewolf team consists of: ", nameText, "."]
  pm name msg
  pure ()



-- | Un-hex all players.
removeHexed :: MonadWG m => m ()
removeHexed = forPlayers [hasModifier Hexed] $ \Player{name} ->
  selectPlayer name % #modifiers %= filter (/= Hexed)

-- | Perform the list of night actions in the given order.
performActions :: forall m. MonadWG m => [NightAction m] -> m ()
performActions actions = do
  -- send out all of the prompts for actions
  forM_ actions $ \NightAction{actionPrompt, predicate} ->
    case actionPrompt of
      Just msg -> forPlayers [predicate] $ \Player{name} -> pm name msg
      Nothing -> pure ()
  -- actually get the actions
  forM_ actions $ \NightAction{predicate, nightAction} ->
    forPlayers [predicate] nightAction

-- | Take first night actions.
firstNightActions :: MonadWG m => m ()
firstNightActions = performActions $
  [ -- The minion goes before the turncoat so that the turncoat isn't included
    -- in the list of werewolf team players.
    minionFirstNight
    -- The turncoat goes first so that their team is established for any other
    -- actions.
  , turncoatFirstNight
  , seer
  , prophet
  , mentalist
  , mystic
  ]

-- | Take normal nightly actions.
nightlyActions :: MonadWG m => m ()
nightlyActions = performActions $
  [ -- The turncoat goes first because they go first on the first night.
    turncoatNightly
  , seer
  , prophet
  , mentalist
  , mystic
  -- SOME HERE
  , revealer
  , werewolf
  -- , doctor
  ]
