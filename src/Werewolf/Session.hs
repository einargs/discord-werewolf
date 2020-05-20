module Werewolf.Session
  ( ActionRequest(..)
  , ActionResponse(..)
  , MonadCom(..)
  , SuspendSession(..)
  , Session
  , Suspension
  , startSession
  , stepSession
  ) where

import Control.Monad.Coroutine
import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Identity
import Data.Text (Text)

import Werewolf.Player
import Werewolf.Game (Game, ActionInfo, Action, Message, Scope)

-- | A prompt for the player to enter a command.
data ActionRequest = ActionRequest
  { requestTarget :: PlayerName
  , requestMessage :: Text
  , responsePredicate :: (ActionInfo -> Bool)
  }

-- | A response sent back when the player has performed a
-- prompted command.
data ActionResponse = ActionResponse
  { wasPromptRequired :: Bool
  -- ^ used to determine whether or not the prompt was sent and needs to be logged.
  , actionTaken :: ActionInfo
  -- ^ the information of the action taken.
  , actionScope :: Scope
  -- ^ The scope the action was taken in (DMs or public chat).
  }

class (Monad m) => MonadCom m where
  sendMessage :: Message -> m ()
  -- | Returns whether the player needed to be prompted and the result.
  sendRequest :: ActionRequest -> m ActionResponse
  -- | Get the next action to be processed.
  getAction :: m Action

data SuspendSession a
  = RequestAction ActionRequest (ActionResponse -> a)
  | AwaitAction (Action -> a)
  | SendMessage Message a
  deriving (Functor)

type InternalSession = RandT StdGen (StateT Game (Coroutine SuspendSession Identity))
  --Coroutine SuspendSession (StateT Game (Rand StdGen))
newtype Session a = Session { unSession :: InternalSession a }
  deriving (Functor, Applicative, Monad)

deriving instance MonadState Game InternalSession => MonadState Game Session
deriving instance MonadRandom InternalSession => MonadRandom Session

newtype Suspension a = Suspension { unSuspension :: Coroutine SuspendSession Identity a }

instance MonadCom Session where
  sendMessage m = Session $ lift $ lift $
    suspend $ SendMessage m (pure ())
  sendRequest req = Session $ lift $ lift $
    suspend $ RequestAction req pure
  getAction = Session $ lift $ lift $
    suspend $ AwaitAction pure

startSession :: StdGen -> Game -> Session a -> Suspension a
startSession gen game = Suspension . flip evalStateT game . flip evalRandT gen . unSession

stepSession :: Suspension a -> Either (SuspendSession (Suspension a)) a
stepSession m = case runIdentity $ resume $ unSuspension m of
  Left s -> Left $ Suspension <$> s
  Right v -> Right v
