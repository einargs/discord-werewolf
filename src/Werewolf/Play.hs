module Werewolf.Play where

import Control.Monad.Random
import Control.Monad.State.Class

import Optics (view, (%), Lens', lens)
import Optics.State.Operators ((%=), (.=))

import qualified Data.Text as T

import Werewolf.Game
import Werewolf.Player

-- | Although mechanically it is possible for a game to
-- not contain a player with a given player name, part
-- of the contract for the PlayerName type is that
-- it only wraps userids involved in a game.
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

class (Monad m) => MonadCom m where
  sendMessage :: Message -> m ()
  -- | Returns whether the player needed to be prompted and the result.
  sendRequest :: PlayerName -> T.Text -> (ActionInfo -> Bool) -> m (Bool, ActionInfo)

type MonadWG m = (MonadState Game m, MonadRandom m, MonadCom m)

addEvent :: (MonadState Game m) => Event -> m ()
addEvent e = #rounds %= f where
    f = case e of
      NightEnd -> endNight . appendEvent
      _ -> appendEvent

    appendEvent :: [Round] -> [Round]
    appendEvent [] = [Round [e]]
    appendEvent ((Round events):rest) = Round (e:events):rest

    endNight :: [Round] -> [Round]
    endNight = (Round []:)

-- | Only use this to send messages; it makes sure to log the message
-- in the event log.
handleMessage :: (MonadState Game m, MonadCom m) => Message -> m ()
handleMessage msg = do
  addEvent $ SendMessage msg
  sendMessage msg

-- | Only use this to request that the player take some action; it makes
-- sure to log the prompt message if it's needed.
--
-- Note that this does NOT log or process the resulting event; that should
-- be handled with the usual process.
requestAction :: (MonadState Game m, MonadCom m) => PlayerName -> T.Text -> (ActionInfo -> Bool) -> m Action
requestAction name txt p = do
  (neededPrompt, info) <- sendRequest name txt p
  when neededPrompt $ do
    addEvent $ SendMessage $ PM name txt
  pure $ Action name info

pm :: MonadWG m => PlayerName -> T.Text -> m ()
pm pn t = handleMessage $ PM pn t

announce :: MonadWG m => T.Text -> m ()
announce = handleMessage . Announce

queryPlayers :: (MonadState Game m) => (Player -> Bool) -> m [Player]
queryPlayers q = gets (filter q . view #players)

killPlayer :: (MonadState Game m) => PlayerName -> m ()
killPlayer name = do
  currentStatus <- gets $ view statusLens
  when (currentStatus == Dead) $ error "Attempting to kill an already dead player"
  statusLens .= Dead
  where statusLens = selectPlayer name % #status

isRoleActive :: (MonadState Game m) => Role -> m Bool
isRoleActive role = not . null <$> queryPlayers (hasRole role)

forAny :: (MonadState Game m) => Role -> (Player -> m ()) -> m ()
forAny role f = queryPlayers (hasRole role) >>= mapM_ f

gameStart :: MonadWG m => m ()
gameStart = pure ()

nightStart :: MonadWG m => m ()
nightStart = do
  -- Log that the day has ended
  addEvent DayEnd
  -- Tell any mystics how many werewolf players there are
  forAny Mystic $ \player -> do
    werewolfPlayers <- queryPlayers $ onTeam WerewolfTeam
    let countText = T.pack $ show $ length werewolfPlayers
    pm (name player) $ T.concat ["There are ", countText, " players on the werewolf team."]

dayStart :: MonadWG m => m ()
dayStart = do
  -- Log that the night has ended
  addEvent NightEnd

process :: (MonadWG m) => Action -> m ()
process (Action pname info) = case info of
  _ -> pure ()
