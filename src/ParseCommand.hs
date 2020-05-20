module ParseCommand
  ( parseCommand
  , CommandError(..)
  ) where

import Data.Text (Text)
import Control.Monad
import Control.Monad.Reader
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Control.Arrow (left)
import Control.Applicative (Alternative)
import Data.Functor (($>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Discord.Types (Snowflake(..), UserId)

import Werewolf.Player (Team(..), PlayerName(..), Role(..))
import Werewolf.Game (ActionInfo(..))
import Command

newtype Parser a = Parser
  { unParser :: ParsecT CommandError Text (Reader (UserId -> Bool)) a
  } deriving
  ( Functor, Applicative, Monad
  , Alternative, MonadPlus
  , MonadReader (UserId -> Bool)
  , MonadParsec CommandError Text
  )

team :: Parser Team
team = string "werewolf" $> WerewolfTeam
  <|> string "villager" $> VillagerTeam

userMention :: Parser UserId
userMention = do
  void $ string "<@!"
  n <- L.decimal
  void $ char '>'
  return $ Snowflake n

playerName :: Parser PlayerName
playerName = do
  uid <- userMention
  isPlayer <- ask
  unless (isPlayer uid) $ customFailure $ UserNotPlaying uid
  return $ PlayerName uid

yesOrNo :: Parser Bool
yesOrNo = (string "yes" $> True)
  <|> (string "no" $> False)

werewolfRole :: Parser Role
werewolfRole = role "werewolf" Werewolf
  <|> role "werecub" Werecub
  <|> role "werekitten" Werekitten
  <|> role "spellcaster" Spellcaster
  <|> role "toughWolf" ToughWolf
  <|> role "traitor" Traitor
  <|> role "warlock" Warlock
  <|> role "doctor" Doctor
  <|> role "seer" Seer
  <|> role "bodyguard" Bodyguard
  <|> role "guardian_angel" GuardianAngel
  <|> role "huntress" Huntress
  <|> role "harlot" Harlot
  <|> role "hunter" Hunter
  <|> role "mentalist" Mentalist
  <|> role "mad_scientist" MadScientist
  <|> role "cupid" Cupid
  <|> role "mystic" Mystic
  <|> role "prophet" Prophet
  <|> role "revealer" Revealer
  <|> role "lycan" Lycan
  <|> role "mason" Mason
  <|> role "gunner" Gunner
  <|> role "prince" Prince
  <|> role "doppelganger" Doppelganger
  <|> role "monster" Monster
  <|> role "turncoat" Turncoat
  <|> role "villager" Villager
  where role t r = string t $> r

arg :: Parser a -> Parser a
arg = (space1 >>)

werewolfAction :: Parser ActionInfo
werewolfAction = action "accuse" Accuse <*> playerName
  <|> action "vote" LynchVote <*> arg yesOrNo
  <|> action "kill" WerewolfKill <*> arg playerName
  <|> action "hex" SpellcasterHex <*> arg playerName
  <|> action "revive" DoctorRevive <*> arg playerName
  <|> action "see" SeerClairvoyance <*> arg playerName
  <|> action "guard" BodyguardProtect <*> arg playerName
  <|> action "protect" GuardianAngelProtect <*> arg playerName
  <|> action "hunt" HuntressKill <*> arg playerName
  <|> action "hideWith" HarlotHideWith <*> arg playerName
  <|> action "revenge" HunterRevenge <*> arg playerName
  <|> action "compare" MentalistCompare <*> arg playerName <*> arg playerName
  <|> action "arrow" CupidArrow <*> arg playerName <*> arg playerName
  <|> action "vision" ProphetVision <*> arg werewolfRole
  <|> action "reveal" RevealerKill <*> arg playerName
  <|> action "masonReveal" MasonReveal
  <|> action "shoot" GunnerShoot <*> arg playerName
  <|> action "doppelgang" DoppelgangerChoose <*> arg playerName
  <|> action "switch" TurncoatSwitch <*> arg team
  where action t a = string t $> a

commandPhrase :: Parser Command
commandPhrase = WerewolfAction <$> werewolfAction
  <|> string "start" $> StartGame
  <|> string "add" $> AddPlayer <*> arg userMention
  <|> string "remove" $> RemovePlayer <*> arg playerName
  <|> string "join" $> Join
  <|> string "leave" $> Leave

command :: Parser Command
command = do
  werewolfPrefix
  space1
  commandPhrase

werewolfPrefix :: Parser ()
werewolfPrefix = void $ string "!w"

runMyParser
  :: (UserId -> Bool)
  -> Parser a
  -> Text
  -> Either (ParseErrorBundle Text CommandError) a
runMyParser isPlayer p t = flip runReader isPlayer $ run $ unParser p
  where run m = runParserT m "" t

parseCommand
  :: Scope
  -> (UserId -> Bool)
  -> Text
  -> Either CommandError Command
parseCommand scope isPlayer =
  (>>= requireScope scope)
    . left convertError
    . runMyParser isPlayer command
  where
    f :: ErrorFancy CommandError -> CommandError -> CommandError
    f ef = \case
      UnrecognizedCommand -> case ef of
        ErrorCustom ce -> ce
        _ -> UnrecognizedCommand
      e -> e
    convertError bundle = case NE.head $ bundleErrors bundle of
      FancyError _ set -> S.foldr' f UnrecognizedCommand set
      _ -> UnrecognizedCommand
