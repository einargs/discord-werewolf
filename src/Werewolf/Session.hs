module Werewolf.Session
  ( MonadCom(..)
  , SuspendSession(..)
  , MonadVictory(..)
  , Session
  , Suspension
  , startSession
  , stepSession
  ) where

import Control.Monad.Coroutine
import Control.Monad.State
import Control.Monad.Random
import Data.Text (Text)

import Werewolf.Player
import Werewolf.Game (Game, ActionInfo, Action, Message, Scope, Victory)

-- | A type class representing a monad's capability to send
-- messages to players and the public chat as well as get the
-- next action to be processed.
class (Monad m) => MonadCom m where
  -- | Send a message.
  sendMessage :: Message -> m ()
  -- | Get the next action to be processed.
  getAction :: m Action

-- | A type class representing a monad's capability to short-circuit
-- and declare a victory for a given team.
class (Monad m) => MonadVictory m where
  exitWithVictory :: Victory -> m a

data SuspendSession a
  = AwaitAction (Action -> a)
  | SendMessage Message a
  deriving (Functor)

type SessionResult = (Victory, Game)

type InternalSession = RandT StdGen (StateT Game (Coroutine SuspendSession (Either SessionResult)))
newtype Session a = Session { unSession :: InternalSession a }
  deriving (Functor, Applicative, Monad)

deriving instance MonadState Game InternalSession => MonadState Game Session
deriving instance MonadRandom InternalSession => MonadRandom Session

instance MonadCom Session where
  sendMessage m = Session $ lift $ lift $
    suspend $ SendMessage m (pure ())
  getAction = Session $ lift $ lift $
    suspend $ AwaitAction pure

instance MonadVictory Session where
  exitWithVictory vic = do
    game <- get
    Session $ lift $ lift $ lift $ Left (vic, game)

newtype Suspension = Suspension {
  unSuspension :: (Coroutine SuspendSession (Either SessionResult)) SessionResult
  }

startSession :: StdGen -> Game -> Session Victory -> Suspension
startSession gen game = Suspension . flip runStateT game . flip evalRandT gen . unSession

stepSession :: Suspension -> Either (SuspendSession Suspension) (Victory, Game)
stepSession m = case resume $ unSuspension m of
  Left res -> Right res
  Right step -> case step of
    Left sus -> Left $ Suspension <$> sus
    Right res -> Right res
