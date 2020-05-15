module Run
  ( run, Config(..)
  ) where

import Discord
import Discord.Types
import qualified Discord.Requests as R
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Reader
import Data.List (foldl')
import Data.IORef

import Handler

run :: Config -> IO ()
run config = do
  configRef <- newIORef config
  userFacingError <- runDiscord def
    { discordToken = T.concat ["Bot ", botToken config]
    , discordOnEvent = eventHandler config }
  TIO.putStrLn userFacingError

data MessageClass
  = Public Message
  | DM PlayerInfo Message
  | Unrelated

isWerewolfMessage :: Message -> Bool
isWerewolfMessage m = notBot && hasPrefix where
  notBot = not $ userIsBot $ messageAuthor m
  hasPrefix = "!w " `T.isPrefixOf` T.toLower (messageText m)

classify :: [PlayerInfo] -> ChannelId -> Message -> MessageClass
classify players mainChannel m
  | isWerewolfMessage m =
    if isPublic
      then Public m
      else dmOrUnrelated
  | otherwise = Unrelated
  where
    cid = messageChannel m
    isPublic = cid == mainChannel
    dmOrUnrelated = foldl' f Unrelated players
    f :: MessageClass -> PlayerInfo -> MessageClass
    f _ p | pmChannel p == cid = DM p m
          | otherwise = Unrelated

eventHandler :: Config -> DiscordHandle -> Event -> IO ()
eventHandler config dis event = runHandler config $ do
  hputStrLn $ T.concat ["event: ", T.pack $ show event]
  case event of
    Ready _version _user _channels _guilds _sessionId ->
      pure ()
    MessageCreate m -> do
      players <- readPlayers
      mainChannel <- asks werewolfChannel
      case classify players mainChannel m of
        Unrelated -> hputStrLn "Got unrelated message."
        Public _m -> respond m "Got public command!"
        DM _pi _m -> respond m "Got private command!"
    _ -> pure ()
  where
    respond :: Message -> T.Text -> Handler ()
    respond m t = liftIO $ restCall dis (R.CreateMessage (messageChannel m) t) >> pure ()
