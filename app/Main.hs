module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Run
import Discord.Types (ChannelId, Snowflake(..))

testWerewolfChannel :: ChannelId
testWerewolfChannel = Snowflake 710195846318391347

main :: IO ()
main = do
  TIO.putStrLn "Please paste the authentication token below:"
  token <- TIO.getLine
  TIO.putStrLn $ T.concat ["Read: ", token]
  run $ InitialConfig
    { werewolfChannel = testWerewolfChannel
    , botToken = token
    }
