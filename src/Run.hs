module Run
  ( run, InitialConfig(..)
  ) where

import Discord
import Discord.Types
import qualified Discord.Requests as R
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Reader
import Data.List (foldl')

import Handler
import Util.Optics
import Util.Text
import Werewolf.Game (Action(..))
import Werewolf.Player (PlayerName(..))
import Command
import ParseCommand

run :: InitialConfig -> IO ()
run config = do
  env <- initEnv config
  userFacingError <- runDiscord def
    { discordToken = botAuth env
    , discordOnEvent = eventHandler env }
  TIO.putStrLn userFacingError

data MessageClass
  = Group Message
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
      then Group m
      else dmOrUnrelated
  | otherwise = Unrelated
  where
    cid = messageChannel m
    isPublic = cid == mainChannel
    dmOrUnrelated = foldl' f Unrelated players
    f :: MessageClass -> PlayerInfo -> MessageClass
    f _ p | pmChannel p == cid = DM p m
          | otherwise = Unrelated

hclassify :: Message -> Handler MessageClass
hclassify m = do
  players <- readPlayers
  mainChannel <- oask #werewolfChannel
  pure $ classify players mainChannel m

mention :: UserId -> T.Text
mention uid = T.concat ["<@!", tshow uid, ">"]

respond :: DiscordHandle -> Message -> [T.Text] -> Handler ()
respond dis m ts = void $ hrestCall dis
  (R.CreateMessage (messageChannel m) (T.concat ts))

reply :: DiscordHandle -> Message -> [T.Text] -> Handler ()
reply dis m ts = let
  sender = mention $ userId $ messageAuthor m
  in respond dis m $ sender : ": " : ts

data CommandClass
  = NotCommand
  | MalformedCommand CommandError
  | CorrectCommand Command

commandFrom :: Message -> Handler CommandClass
commandFrom m@Message{messageText, messageAuthor} = do
  players <- readPlayers
  let isPlayer uid = uid `elem` (playerId <$> players)
      parse scope = case parseCommand scope isPlayer messageText of
        Left err -> MalformedCommand err
        Right cmd -> CorrectCommand cmd
  classification <- hclassify m
  case classification of
    Group _m -> pure $ parse Public
    DM _pi _m -> pure $ parse Private
    Unrelated -> pure NotCommand

handleCommand :: DiscordHandle -> Message -> Command -> Handler ()
handleCommand dis m = \case
  WerewolfAction a -> reply dis m ["got action ", tshow a]
  AddPlayer uid -> do
    addPlayer dis uid
    reply dis m ["added ", mention uid, " to the game."]
  RemovePlayer (PlayerName uid) -> do
    removePlayer uid
    reply dis m ["removed ", mention uid, " from the game."]
  Join -> do
    addPlayer dis $ userId $ messageAuthor m
    reply dis m ["you have joined the game."]
  Leave -> do
    removePlayer $ userId $ messageAuthor m
    reply dis m ["you have left the game."]
  StartGame -> respond dis m ["Starting game."]

handleMessage :: DiscordHandle -> Message -> Handler ()
handleMessage dis m = do
  cmdClass <- commandFrom m
  case cmdClass of
    NotCommand -> hputStrLn "Got unrelated message."
    MalformedCommand err -> case err of
      UserNotPlaying uid -> reply dis m [mention uid, " is not playing"]
      UnrecognizedCommand -> reply dis m ["we could not understand your command"]
      ScopeShouldBe scope -> case scope of
        Public -> reply dis m ["command must be sent privately in a DM to the bot."]
        Private -> reply dis m ["command must be sent publically in the main channel."]
    CorrectCommand cmd -> handleCommand dis m cmd

eventHandler :: Env -> DiscordHandle -> Event -> IO ()
eventHandler env dis event = runHandler env $ do
  hputStrLn $ T.concat ["event: ", tshow event]
  case event of
    Ready _version _user _channels _guilds _sessionId ->
      pure ()
    MessageCreate m -> handleMessage dis m
    _ -> pure ()
