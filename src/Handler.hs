module Handler
  ( PlayerInfo(..)
  , Env(..)
  , InitialConfig(..)
  , Handler
  , initEnv
  , botAuth , runHandler
  , readPlayers
  , hmodify
  , addPlayer
  , removePlayer
  , hputStrLn
  , hrestCall
  ) where

import Discord.Types
import Discord
import qualified Discord.Requests as R
import Discord.Internal.Rest (Request)
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels)

data PlayerInfo = PlayerInfo
  { playerId :: UserId
  , pmChannel :: ChannelId
  }

data InitialConfig = InitialConfig
  { botToken :: T.Text
  , werewolfChannel :: ChannelId
  }
makeFieldLabelsWith noPrefixFieldLabels ''InitialConfig

data Env = Env
  { werewolfChannel :: ChannelId
  , playersInfo :: TVar [PlayerInfo]
  , botToken :: T.Text
  }
makeFieldLabelsWith noPrefixFieldLabels ''Env

botAuth :: Env -> T.Text
botAuth Env{botToken} = T.concat ["Bot ", botToken]

initEnv :: InitialConfig -> IO Env
initEnv InitialConfig{botToken, werewolfChannel} = do
  playersInfo <- newTVarIO []
  return Env {..}

newtype Handler a = Handler { unHandler :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadFail)

runHandler :: Env -> Handler a -> IO a
runHandler e = flip runReaderT e . unHandler

readPlayers :: Handler [PlayerInfo]
readPlayers = do
  ps <- asks playersInfo
  liftIO $ readTVarIO ps

hmodify :: (Env -> TVar a) -> (a -> a) -> Handler ()
hmodify e f = asks e >>= (liftIO . atomically . flip modifyTVar' f)

hrestCall :: (FromJSON a, Request (r a), MonadIO m) => DiscordHandle -> r a -> m a
hrestCall dis req = do
  result <- liftIO $ restCall dis req
  case result of
    Left err ->
      -- TODO: replace this with some proper error handling; maybe retry the request?
      liftIO $ fail $ "Rest call failed" ++ show err
    Right n -> pure n

addPlayer :: DiscordHandle -> UserId -> Handler ()
addPlayer dis uid = do
  ChannelDirectMessage{channelId} <- hrestCall dis (R.CreateDM uid)
  let p = PlayerInfo { playerId = uid, pmChannel = channelId }
  hmodify playersInfo (p:)

removePlayer :: UserId -> Handler ()
removePlayer uid = hmodify playersInfo f where
  f = filter $ (uid/=) . playerId

hputStrLn :: T.Text -> Handler ()
hputStrLn = liftIO . TIO.putStrLn
