module Handler
  ( PlayerInfo(..)
  , Config(..)
  , Handler
  , runHandler
  , readPlayers
  , hmodify
  , addPlayer
  , hputStrLn
  ) where

import Discord.Types
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data PlayerInfo = PlayerInfo
  { userId :: UserId
  , pmChannel :: ChannelId
  }

data Config = Config
  { werewolfChannel :: ChannelId
  , playersInfo :: TVar [PlayerInfo]
  , botToken :: T.Text
  }

newtype Handler a = Handler { unHandler :: ReaderT Config IO a }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO)

runHandler :: Config -> Handler a -> IO a
runHandler c = flip runReaderT c . unHandler

readPlayers :: Handler [PlayerInfo]
readPlayers = do
  ps <- asks playersInfo
  liftIO $ readTVarIO ps

hmodify :: (Config -> TVar a) -> (a -> a) -> Handler ()
hmodify e f = asks e >>= (liftIO . atomically . flip modifyTVar' f)

addPlayer :: PlayerInfo -> Handler ()
addPlayer p = hmodify playersInfo (p:)

hputStrLn :: T.Text -> Handler ()
hputStrLn = liftIO . TIO.putStrLn
