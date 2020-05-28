module Werewolf.Play.Base
  ( MonadCom
  , MonadVictory
  , selectPlayer
  , MonadWG
  , queryPlayers
  , killPlayerWithReaction
  , requestAction
  , requestOptionalAction
  , nextAction
  , pm
  , announce
  , isRoleActive
  , forAny
  , currentRoundEvents
  , endRound
  , roundCount
  , dayToNight
  , clearActionBuffer
  ) where

import Control.Monad.Random
import Control.Monad.State.Class
import Control.Applicative

import Optics (view, (%), Lens', lens, (^.))
import Data.Sequence.Optics (viewL)
import qualified Data.Sequence as S
import Data.Maybe (isJust)
import Data.Sequence ((|>), ViewL(..))
import Optics.State.Operators ((%=), (.=))

import qualified Data.Text as T

import Util.Optics (getv)
import Util.Text
import Werewolf.Game
import Werewolf.Player
import qualified Werewolf.Descriptions as Desc
import Werewolf.Session (MonadCom(..), MonadVictory(..))

-- | Although mechanically it is possible for a game to
-- not contain a player with a given player name, part
-- of the contract for the PlayerName type is that
-- it only wraps UserIds involved in a game.
selectPlayer :: PlayerName -> Lens' Game Player
selectPlayer pname = #players % lens g s where
  g :: [Player] -> Player
  g [] = error "PlayerName should be in Game"
  g (p:ps) = if view #name p == pname
    then p
    else g ps

  s :: [Player] -> Player -> [Player]
  s []  _ = error "PlayerName should be in Game"
  s (p:ps) p' = if view #name p == pname
    then p':ps
    else p:s ps p'

type MonadWG m = (MonadState Game m, MonadRandom m, MonadCom m, MonadVictory m)

addEvent :: (MonadState Game m) => Event -> m ()
addEvent e = #rounds %= appendEvent where
    appendEvent [] = error "The rounds list should never be empty."
    appendEvent ((Round events):rest) = Round (e:events):rest

-- | Deque an action matching the predicate from the front of the action
-- queue if it is present.
searchAction
  :: MonadState Game m
  => (Action -> Maybe a)
  -> m (Maybe a)
searchAction actionPred = do
  buffer <- getv #actionBuffer
  let mbIdx = S.findIndexL (isJust . actionPred) buffer
  let mbAction = mbIdx >>= (`S.lookup` buffer)
  case mbIdx of
    Just idx -> #actionBuffer %= S.deleteAt idx
    Nothing -> pure ()
  -- If the index is present, that means that the predicate
  -- returned `Just`, and thus the value at that location
  -- (a) is present and (b) when passed to `actionPred` will
  -- result in `Just`.
  pure $ mbAction >>= actionPred

-- | Deques an action from the front of the buffer if there is
-- an action in the buffer.
dequeueAction :: MonadState Game m => m (Maybe Action)
dequeueAction = do
  vl <- getv $ #actionBuffer % viewL
  case vl of
    EmptyL -> pure Nothing
    a :< rest -> do
      #actionBuffer .= rest
      pure $ Just a

-- | Get the next action in the buffer. If no action is in the buffer,
-- get an action using MonadCom.
nextAction :: (MonadState Game m, MonadCom m) => m Action
nextAction = do
  mbAct <- dequeueAction
  case mbAct of
    Nothing -> getAction
    Just act -> pure act

-- | Enque an action at the back of the queue.
enqueueAction :: MonadState Game m => Action -> m ()
enqueueAction action = #actionBuffer %= (|> action)

-- | Only use this to send messages; it makes sure to log the message
-- in the event log.
handleMessage :: (MonadState Game m, MonadCom m) => Message -> m ()
handleMessage msg = do
  addEvent $ SendMessage msg
  sendMessage msg

-- | Use `playerCan` to validate an action; returns true if there
-- was no error, and false if there was.
--
-- If there is an error, this will pm the player the error message.
validateAction :: (MonadCom m, MonadState Game m) => Action -> m Bool
validateAction action@Action{playerName} = do
  actionTaker <- getv $ selectPlayer playerName
  game@Game{currentPhase, rounds} <- get
  let roundNumber = length rounds
      getPlayerStatus name = game ^. selectPlayer name % #status
      capability = playerCan
        currentPhase
        roundNumber
        getPlayerStatus
        actionTaker
        action
  case capability of
    Allowed -> pure True
    CannotBecause err -> do
      pm playerName $ Desc.capabilityError err
      pure False

-- | Request that a certain player take an action.
--
-- This will check the action buffer to see if the requested action
-- is already in the buffer.
--
-- Note that this does NOT log or process the resulting action; that should
-- be handled with the usual process.
requestAction
  :: forall m a. (MonadState Game m, MonadCom m)
  => PlayerName
  -> T.Text
  -> (Action -> Maybe a)
  -> m a
requestAction name txt basePred = do
  -- We check if a matching action is currently in the buffer.
  mbResult <- search'
  case mbResult of
    -- If the action is in the queue
    Just result -> pure result
    -- If it isn't, we prompt for it and start requesting more actions.
    Nothing -> pm name txt >> getUntil
  where
    -- | This searches for a matching and valid action
    -- in the buffer.
    search' :: m (Maybe a)
    search' = searchAction searchPred >>= \case
      Just (act, v) -> validateAction act >>= \case
        True -> pure $ Just v
        False -> search'
      Nothing -> search'

    -- | This predicate is used by `search'`.
    searchPred :: Action -> Maybe (Action, a)
    searchPred act = (act,) <$> actionPred act

    -- | A modified version of basePred that makes sure
    -- to also check that the action taker is the given
    -- player.
    actionPred :: Action -> Maybe a
    actionPred act@Action{playerName}
      | name == playerName = basePred act
      | otherwise = Nothing

    -- | If we can't get the action from the buffer, we keep requesting new
    -- actions until we get one that matches.
    getUntil :: m a
    getUntil = do
      act <- getAction
      let cont = enqueueAction act >> getUntil
      case actionPred act of
        Just v -> validateAction act >>= \case
          True -> pure v
          False -> cont
        Nothing -> cont

-- | Request that a certain player take an optional action. An action
-- requested with this can be skipped using the pass command; if that
-- happens, this will return `Nothing`.
--
-- See `requestAction` for more information.
requestOptionalAction
  :: forall a m. (MonadState Game m, MonadCom m)
  => PlayerName
  -> T.Text
  -> (Action -> Maybe a)
  -> m (Maybe a)
requestOptionalAction name txt basePred =
  requestAction name txt actionPred
  where
    actionPred :: Action -> Maybe (Maybe a)
    actionPred act@Action{actionInfo} = case actionInfo of
          Pass -> Just Nothing
          _ -> Just <$> basePred act

-- | Direct message/personal message a player with a message.
pm :: (MonadState Game m, MonadCom m) => PlayerName -> T.Text -> m ()
pm pn t = handleMessage $ PM pn t

-- | Send a public message in the werewolf channel.
announce :: (MonadState Game m, MonadCom m) => T.Text -> m ()
announce = handleMessage . Announce

-- | Get a list of alive players satisfying the predicates.
queryPlayers :: (MonadState Game m) => [Player -> Bool] -> m [Player]
queryPlayers qs = gets (filter q . view #players)
  where q = and <$> sequence (isAlive:qs)

-- | Kill the specific player, call the reaction passed to the function,
-- and then check the win conditions after the reaction.
--
-- NOTE: will error if the player is already dead.
killPlayerWithReaction
  :: (MonadState Game m, MonadCom m, MonadVictory m)
  => PlayerName
  -> (PlayerName -> m ())
  -> m ()
killPlayerWithReaction name reaction = do
  currentStatus <- getv statusLens
  when (currentStatus == Dead) $ error "Attempting to kill an already dead player"
  statusLens .= Dead
  addEvent $ PlayerDeath name
  pm name "You have died."
  reaction name
  checkWinConditions
  where statusLens = selectPlayer name % #status

-- | Check if any team has won yet.
checkWinConditions :: forall m. (MonadState Game m, MonadCom m, MonadVictory m) => m ()
checkWinConditions = determineIfVictory >>= \case
    Nothing -> pure ()
    Just vic -> declareVictory vic
  where
    awardUnlessMonster :: Victory -> m Victory
    awardUnlessMonster vic = isRoleActive Monster >>= \case
      True -> pure MonsterVictory
      False -> pure vic
    -- | The werewolves win if the werewolf is alive and
    -- only one villager is alive.
    isWerewolfVictory :: m Bool
    isWerewolfVictory = liftA2 (&&)
      (isRoleActive Werewolf)
      ((<=1) . length <$> queryPlayers [onTeam VillagerTeam])
    -- | The villagers win
    isVillagerVictory :: m Bool
    isVillagerVictory = not <$> isRoleActive Werewolf
    determineIfVictory :: m (Maybe Victory)
    determineIfVictory = isWerewolfVictory >>= \case
      True -> Just <$> awardUnlessMonster WerewolfVictory
      False -> isVillagerVictory >>= \case
        True -> Just <$> awardUnlessMonster VillagerVictory
        False -> pure Nothing

-- | Declares that a team won the game.
declareVictory :: (MonadState Game m, MonadCom m, MonadVictory m) => Victory -> m ()
declareVictory vic = do
  let msg = case vic of
        WerewolfVictory -> "The werewolf team has won."
        VillagerVictory -> "The villager team has won."
        MonsterVictory -> "The monster has won."
  announce msg
  announce "The game is now over."
  exitWithVictory vic

-- | Check if a role is in play or not.
isRoleActive :: (MonadState Game m) => Role -> m Bool
isRoleActive role = not . null <$> queryPlayers [hasRole role]

-- |
roundCount :: (MonadState Game m) => m Int
roundCount = length <$> getv #rounds

-- | Perform an action for any players with the given role.
forAny :: (MonadState Game m) => Role -> (Player -> m ()) -> m ()
forAny role f = queryPlayers [hasRole role] >>= mapM_ f

-- | Get the list of events for the current round.
currentRoundEvents :: MonadState Game m => m [Event]
currentRoundEvents = do
  rounds <- getv #rounds
  case rounds of
    [] -> error "A game should never have an empty rounds field."
    (Round events):_ -> pure events

-- | End the current round and start a new one.
endRound :: (MonadState Game m, MonadCom m) => m ()
endRound = do
  #rounds %= ((Round []):)
  announce "The round is over. Day has risen."

-- | Transition from day to night.
dayToNight :: (MonadState Game m, MonadCom m) => m ()
dayToNight = do
  addEvent NightStart
  announce "Night has fallen."

-- | Clear the action buffer of actions and send a message to players
-- saying the action was rejected.
clearActionBuffer :: (MonadState Game m, MonadCom m) => m ()
clearActionBuffer = do
  buffer <- getv #actionBuffer
  forM_ buffer $ \Action{playerName} ->
    pm playerName "Your extra actions have been rejected."
  #actionBuffer .= S.empty
