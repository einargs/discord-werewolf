module Werewolf.Play.Roles
  ( removeHexed
  , removeProtected
  , nightlyActions
  , firstNightActions
  , AccusationStatus(..)
  , handleDayAction
  ) where

import Control.Monad.Random
import Control.Monad.State.Class
import Control.Exception (assert)

import Optics ((^.), (%), view, mapping)
import Optics.State.Operators ((%=), (.=))

import Data.Text (Text)
import Data.Kind (Type)
import Data.Functor (($>))

import qualified Data.Text as T

import Util.Text (tshow)
import Util.Optics (getv)
import Util.Maybe (whenJust)
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
          | deadName == target ->
              (player ^. #name) `assumeRoleOf` target
        _ -> pure ()

-- | This function implements the cupid's reactions to a death.
cupidReactionsToDeath
  :: forall m. (MonadVictory m, MonadCom m, MonadState Game m)
  => PlayerName -> m ()
cupidReactionsToDeath deadName =
  forAllPlayers [hasRole Cupid] $ \player ->
    when (notHexed player) $
      case player ^. #roleData of
        CupidData (Linked p1 p2)
          | p1 == deadName -> killIfAlive p2
          | p2 == deadName -> killIfAlive p1
        _ -> pure ()
  where
    killIfAlive :: PlayerName -> m ()
    killIfAlive pn = do
      live <- isAlive <$> getv (selectPlayer pn)
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
    whenJust mbTarget attackPlayer
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
          , werecubReactionsToDeath
          , traitorReactionsToDeath
          , doppelgangerReactionsToDeath
          ]

-- | Kill the passed player.
killPlayer
  :: (MonadState Game m, MonadCom m, MonadVictory m)
  => PlayerName
  -> m ()
killPlayer pn = killPlayerWithReaction pn roleReactionsToDeath

-- | Check if the player is protected by a bodyguard or guardian
-- angel and kill them if they aren't.
--
-- Returns whether the attack succeeded or failed.
attackPlayer
  :: (MonadState Game m, MonadCom m, MonadVictory m)
  => PlayerName
  -> m ()
attackPlayer name = isPlayerProtected name >>= \case
  True -> pure ()
  False -> killPlayer name

-- | A data type that represents an action taken at night.
data NightAction (m :: Type -> Type) = NightAction
  { actionPrompt :: Player -> m (Maybe Text)
  , predicates :: [Player -> Bool]
  , nightAction :: Player -> m ()
  }

-- | Build the predicate for the `silentAction` and `promptedAction`
-- conveinences.
nightActionPredicate :: Role -> Player -> Bool
nightActionPredicate role player = hasRole role player && notHexed player

-- | Convenience for building a NightAction without a prompt.
silentAction :: Monad m => Role -> (Player -> m ()) -> NightAction m
silentAction role = NightAction
  (const $ pure Nothing)
  [nightActionPredicate role]

-- | Convenience for building a NightAction with a prompt message.
promptedAction :: Monad m => Text -> Role -> (Player -> m ()) -> NightAction m
promptedAction msg role = NightAction
  (const $ pure $ Just msg)
  [nightActionPredicate role]

-- | Convenience for building a NightAction that uses a dynamically
-- generated prompt message.
dynamicallyPromptedAction
  :: Monad m
  => (Player -> m (Maybe Text))
  -> Role
  -> (Player -> m ())
  -> NightAction m
dynamicallyPromptedAction genPrompt role = NightAction
  genPrompt
  [nightActionPredicate role]

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

-- | Setup the turncoat's initial team.
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

-- | Allow the turncoat to change teams every other round.
turncoatNightly :: MonadWG m => NightAction m
turncoatNightly = promptedAction requestMsg Turncoat $ \Player{name, roleData} -> do
  roundNum <- roundCount
  let count =
        case roleData of
          TurncoatData _ (Just count') -> count'
          _ -> error "should have turncoat data with a count."
  -- The turncoat can only go once every other round.
  when (count + 1 < roundNum) $ do
    mbTeam <- requestOptionalAction name $ \Action{actionInfo} ->
      case actionInfo of
        TurncoatSwitch team -> Just team
        _ -> Nothing
    whenJust mbTeam $ setTurncoatTeam name
  where requestMsg = T.concat
          [ "Please select a team to play on. "
          , "Use `!w switch <team>` to select the team. "
          , "Use `!w pass` to skip changing your team if you want."
          ]

-- | The werewolf actions.
werewolf :: MonadWG m => NightAction m
werewolf = dynamicallyPromptedAction mkRequestMsg Werewolf $ \Player{name} -> do
  -- Needs to be inside the do block so that `name` is in scope.
  let kill = \case
        Nothing -> pure ()
        Just target -> killTarget name target
      requestKill =
        requestOptionalAction name getWerewolfKill >>= kill
  requestKill
  killTwice <- didWerecubDie
  when killTwice requestKill
  where
    getWerewolfKill Action{actionInfo} = case actionInfo of
      WerewolfKill target -> Just target
      _ -> Nothing
    killTarget werewolfName target = do
      let lens = selectPlayer target % #roleData
      targetData <- getv lens
      case targetData of
        LycanData Unturned -> do
          lens .= LycanData Turned
          pm werewolfName $ targetWasLycanMsg target
          pm target tellLycanTheyAreTurnedMsg
        MonsterData -> pm werewolfName $ targetWasMonsterMsg target
        _ -> attackPlayer target
    didWerecubDie = do
      mbLynchedPlayer <- searchCurrentRound $ \case
        SuccessfullyLynched n -> Just n
        _ -> Nothing
      case mbLynchedPlayer of
        Nothing -> pure False
        Just lynchedPlayer -> do
          role <- playerRole <$> getv (selectPlayer lynchedPlayer)
          pure $ role == Werecub
    targetWasMonsterMsg target = T.concat
      [ "You attempted to kill ", Desc.mention target, ", but were unable "
      , "to because they are the monster."
      ]
    --couldNotKillTargetMsg target = T.concat
    --  ["You were unable to kill ", Desc.mention target, "."]
    targetWasLycanMsg target = T.concat
      [Desc.mention target, " was a Lycan and has now been turned."]
    tellLycanTheyAreTurnedMsg = "You were attacked by a werewolf and have now been turned."
    mkRequestMsg _ = do
      canKillTwice <- didWerecubDie
      pure $ Just $ if canKillTwice
        then secondKillRequestMsg
        else requestMsg
    requestMsg = T.concat
      [ "Choose who to kill or pass on killing this round. "
      , "Use `!w kill @player` to kill someone. "
      , "Use `!w pass` to skip killing someone."
      ]
    secondKillRequestMsg = T.concat
      [ "Choose who to kill or pass on killing this round. "
      , "The werecub was lynched during this turn, so you may kill twice. "
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
  whenJust mbTarget $ \target -> do
    role <- playerRole <$> getv (selectPlayer target)
    let attackGuilty = do
          pm name $ guiltyWasTargetedMsg target
          attackPlayer target
    case role of
      Werewolf -> attackGuilty
      Monster -> attackGuilty
      _ -> do
        pm name $ innocentWasTargetedMsg target
        killPlayer name
  where
    guiltyWasTargetedMsg target =
      T.concat [Desc.mention target, " is guilty."]
    innocentWasTargetedMsg target =
      T.concat [Desc.mention target, " is innocent; you will now die."]
    requestMsg = T.concat
      [ "As the revealer, you can choose to target one person per night. "
      , "If that person is the werewolf or the monster, they die; otherwise, "
      , "you die. Use `!w reveal @player` to choose who to reveal or `!w pass` "
      , "to skip."
      ]

-- | Tell the minion who is on the werewolf team.
minionFirstNight :: MonadWG m => NightAction m
minionFirstNight = NightAction (const $ pure Nothing) [hasModifier Minion] $ \Player{name} -> do
  werewolfPlayers <- view (mapping #name) <$> queryPlayers [onTeam WerewolfTeam]
  let nameText = T.intercalate ", " $ Desc.mention <$> werewolfPlayers
      msg = T.concat ["The werewolf team consists of: ", nameText, "."]
  pm name msg

-- | Allow the huntress to kill if they haven't yet and want to.
huntress :: MonadWG m => NightAction m
huntress = dynamicallyPromptedAction mkRequestMsg Huntress $ \Player{name, roleData} ->
  case roleData of
    -- Do nothing if the huntress has already killed
    HuntressData True -> pure ()
    -- If the huntress has not yet killed, ask them if they want to
    HuntressData False -> do
      mbTarget <- requestOptionalAction name $ \Action{actionInfo} ->
        case actionInfo of
          HuntressKill target -> Just target
          _ -> Nothing
      whenJust mbTarget $ \target -> do
        attackPlayer target
        selectPlayer name % #roleData .= HuntressData True
    _ -> error "The huntress night action was called on a player who is not a huntress."
  where
    mkRequestMsg Player{roleData} = case roleData of
      HuntressData hasKilled -> pure $ Just $
        if hasKilled
           then alreadyKilledMsg
           else requestMsg
      _ -> error "The prompt creation for the huntress was called on a player that is not a huntress."
    requestMsg = T.concat
      [ "As the huntress, once per game you can choose to kill someone during "
      , "the night. Use `!w hunt @player` to choose to kill someone or `!w pass` "
      , "to skip killing someone tonight."
      ]
    alreadyKilledMsg = T.concat
      [ "As the huntress, you have already killed once in this game. "
      , "You can no longer kill."
      ]

-- | Remove a modifier from all players in the game.
removeModifier :: MonadWG m => Modifier -> m ()
removeModifier modifier = forPlayers [hasModifier modifier] $ \Player{name} ->
  selectPlayer name % #modifiers %= filter (/= modifier)

-- | Un-hex all players.
removeHexed :: MonadWG m => m ()
removeHexed = removeModifier Hexed

-- | Remove the protected status from all players.
removeProtected :: MonadWG m => m ()
removeProtected = removeModifier Protected

-- | Warlock night actions.
warlock :: forall m. MonadWG m => NightAction m
warlock = dynamicallyPromptedAction mkRequestMsg Warlock $
  handleStatus (pure ()) $ \Player{name} -> do
    mbTarget <- requestOptionalAction name $ \Action{actionInfo} ->
      case actionInfo of
        WarlockCurse target -> Just target
        _ -> Nothing
    whenJust mbTarget $ \target -> do
      addModifier Cursed target
      -- Note that the warlock has now cursed someone
      selectPlayer name % #roleData .= WarlockData True
      pm name $ T.concat ["You successfully cursed ", Desc.mention target, "."]
  where
    handleStatus :: m a -> (Player -> m a) -> Player -> m a
    handleStatus hasCursed hasNotCursed player@Player{roleData} =
      case roleData of
        WarlockData True -> hasCursed
        WarlockData False -> hasNotCursed player
        _ -> error "Player must be a warlock."
    mkRequestMsg =
      let m = pure . Just
      in handleStatus (m hasCursedMsg) $ const $ m requestMsg
    requestMsg = T.concat
      [ "As the warlock, once per game you can choose to curse someone during "
      , "the night or day. Use `!w curse @player` to curse someone or `!w pass` "
      , "to skip cursing someone tonight."
      ]
    hasCursedMsg = T.concat
      [ "As the warlock, you have already cursed one person in this game. "
      , "As such, you cannot curse another."
      ]

-- | Spellcaster night actions.
spellcaster :: forall m. MonadWG m => NightAction m
spellcaster = dynamicallyPromptedAction mkRequestMsg Spellcaster $
  handleStatus (pure ()) $ \Player{name} -> do
    mbTarget <- requestOptionalAction name $ \Action{actionInfo} ->
      case actionInfo of
        SpellcasterHex target -> Just target
        _ -> Nothing
    whenJust mbTarget $ \target -> do
        addModifier Hexed target
        -- note that the spellcaster has now cursed someone
        selectPlayer name % #roleData .= SpellcasterData True
        pm name $ T.concat ["You successfully hexed ", Desc.mention target, "."]
  where
    handleStatus :: m a -> (Player -> m a) -> Player -> m a
    handleStatus hasHexed hasNotHexed player@Player{roleData} =
      case roleData of
        SpellcasterData True -> hasHexed
        SpellcasterData False -> hasNotHexed player
    mkRequestMsg =
      let m = pure . Just
      in handleStatus (m hasHexedMsg) $ const $ m requestMsg
    requestMsg = T.concat
      [ "As the spellcaster, once per game you can choose to hex someone during "
      , "the night or day. Use `!w hex @player` to hex someone or `!w pass` "
      , "to skip hexing someone tonight."
      ]
    hasHexedMsg = T.concat
      [ "As the spellcaster, you have already hexed one person in this game. "
      , "As such, you cannot hex another."
      ]

-- | Tell the masons the other members of their team.
masonFirstNight :: forall m. MonadWG m => NightAction m
masonFirstNight = silentAction Mason $ \Player{name} -> do
  masons <- queryPlayers [hasRole Mason]
  let masonNames = view #name <$> masons
      otherMasons = filter (/= name) masonNames
      nameText = T.intercalate ", " $ Desc.mention <$> otherMasons
      msg = T.concat ["The other masons are: ", nameText, "."]
  pm name msg

-- |
bodyguard :: forall m. MonadWG m => NightAction m
bodyguard = promptedAction requestMsg Bodyguard $ \Player{name} -> do
  target <- requestAction name $ \Action{actionInfo} ->
    case actionInfo of
      BodyguardProtect target -> Just target
      _ -> Nothing
  target
  where
    requestMsg = T.concat
      -- TODO: expand
      [ "Please choose someone to bodyguard."]

-- | Perform the list of night actions in the given order.
performActions :: forall m. MonadWG m => [NightAction m] -> m ()
performActions actions = do
  -- send out all of the prompts for actions
  forM_ actions $ \NightAction{actionPrompt, predicates} ->
    forPlayers predicates $ \player@Player{name} ->
      actionPrompt player >>= \case
        Just msg -> pm name msg
        Nothing -> pure ()
  -- actually get the actions
  forM_ actions $ \NightAction{predicates, nightAction} ->
    forPlayers predicates nightAction

-- | Take first night actions.
firstNightActions :: MonadWG m => m ()
firstNightActions = performActions
  [ -- The minion goes before the turncoat so that the turncoat isn't included
    -- in the list of werewolf team players.
    minionFirstNight
    -- The turncoat goes first so that their team is established for any other
    -- actions.
  , turncoatFirstNight
  -- The warlock goes if they want to curse someone so that it affects the seer.
  , warlock
  -- Required actions generally go first.
  , masonFirstNight
  -- , cupidFirstNight
  -- , doppelgangerFirstNight
  , seer
  , prophet
  , mentalist
  , mystic
  -- The protectors go on the first night because their protection
  -- extends into the day.
  -- , bodyguard
  -- , guardianAngel
  ]

-- | Take normal nightly actions.
nightlyActions :: MonadWG m => m ()
nightlyActions = performActions
  [ -- The spellcaster goes before most other roles so that they can pre-empt other
    -- roles (though using the hex at night is kind of a waste).
    spellcaster
  -- The turncoat goes first because they go first on the first night.
  , turncoatNightly
  -- The warlock goes before the seer in case the warlock curses someone the seer
  -- looks at that night.
  , warlock
  , seer
  , prophet
  , mentalist
  , mystic
  -- , bodyguard
  -- , guardianAngel
  -- , harlot
  -- , gunner
  , huntress
  , revealer
  , werewolf
  -- , doctor
  ]

data AccusationStatus
  = UnanimousAccusations
  | MajorityAccusations

handleUnanimousAccusation :: forall m. MonadWG m => PlayerName -> PLayerName -> m ()
handleUnanimousAccusation accuser accused = do
  announce startMsg
  setTimer SecondingAccusationTimer
  waitForSecondLoop []
  where
    waitForSecondLoop seconds = nextAction >>= \case
      TimerInterrupt SecondingAccusationTimer ->
        announce "Accusation has failed; unable to get enough votes."
      TimerInterrupt otherTimer -> enqueueTo #timerBuffer otherTimer
      ActionInterrupt Action{actionInfo, playerName} -> case actionInfo of
        SecondAccusation target
          | accuser == target -> do
              let seconds' = playerName : seconds
              resetTimer SecondingAccusationTimer
              waitForSecondLoop seconds'
          | otherwise -> do
              pm playerName "You may only second the current accusation."
              waitForSecondLoop seconds
        _ -> pm playerName accusationInProgressMsg >> waitForSecondLoop seconds
    accusationInProgressMsg = T.concat
      [ "An accusation is currently in progress; until it is finished you cannot "
      , "perform any other actions."
      ]
    startMsg = T.concat
      [ Desc.mention accuser,
      , " has accused "
      , Desc.mention accused
      , ". All players must unanimously agree with the accusation for it to succeed. "
      , "Use `!w second @accuser` to agree with the accusation. There is a ten second "
      , "time limit that is reset every time someone seconds the accusation."
      ]

-- | Handle an action that happens during the day.
handleDayAction :: forall m. MonadWG m => AccusationStatus -> Action -> m ()
handleDayAction accusationStatus Action{actionInfo, playerName} = case actionInfo of
  Accuse target -> case accusationStatus of
    UnanimousAccusations -> handleUnanimousAccusation playerName target
    MajorityAccusations -> 
  _ -> error "TODO" -- TODO
