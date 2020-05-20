module Werewolf.Start
  (
  ) where

import Data.Maybe (mapMaybe)
import Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels)
import Optics ((%~), ix, (&))
import Control.Monad.Random
import System.Random.Shuffle (shuffleM)

import Werewolf.Player
import Werewolf.Game (Game(..), initGame)

-- | Generates a random value between 0 and 1 and sees if that
-- value is less than the percentage given.
percentage :: MonadRandom m => Double -> m Bool
percentage c = assert (c >= 0 && c <= 1) $ do
  v <- getRandomR (0,1)
  pure $ v < c

-- | Generate a random role.
randomRole :: MonadRandom m => m Role
randomRole = toEnum <$> getRandomR (mini, maxi)
  where
    maxi = fromEnum (maxBound :: Role)
    mini = fromEnum (minBound :: Role)

-- | Generate a random role that satisfies the predicates
-- passed in.
generateRole :: MonadRandom m => [(Role -> Bool)] -> m Role
generateRole ps = do
  r <- randomRole
  if p r
    then pure r
    else generateRole p
  where
    -- Uses the reader monad to collapse all of the predicates
    -- into a single predicate that requires all of them to be
    -- true.
    p = and <$> sequence ps

-- | Check if a role has the same group as any of the roles
-- passed in.
avoidsOverlap :: [Role] -> Role -> Bool
avoidsOverlap roles r = case roleGroup r of
  Nothing -> True
  Just g -> g `elem` groups
    where groups = mapMaybe roleGroup roles

-- | is this a role that should count towards the werewolf
-- team's player count (during initialization only).
werewolfRole :: Role -> Bool
werewolfRole = (WerewolfTeam==) . fst . teamsForRole

-- | Is this a role that should count towards the villager
-- team's player count (during initialization only).
villagerRole :: Role -> Bool
villagerRole = not . werewolfRole

-- | Generate `n` werewolf team roles.
generateWerewolfRoles :: MonadRandom m => GameConfig -> Int -> m [Role]
generateWerewolfRoles GameConfig{} n = assert (n > 0) $ do
  roles <- sequence $ replicate (n-1) gen
  pure $ Werewolf:roles
  where
    -- We always have to have a werewolf, so our generator
    -- for the rest of them excludes werewolves.
    gen = generateRole [(Werewolf/=), werewolfRole]

-- | Generate `n` villager team roles.
generateVillagerRoles :: MonadRandom m => GameConfig -> Int -> [Role]
generateVillagerRoles GameConfig{monsterChance, masonChance} n =
  assert (n > 0) $ do
    monsterPrefix <- includeIf monsterChance [Monster]
    masonsPrefix <- includeIf masonsChance $ replicate masonsCount Mason
    -- We avoid decrementing the `n` and instead just
    -- perform a `take n` on the resulting list in order
    -- to avoid having to worry about `n` becoming negative
    -- if the player count is small enough.
    rest <- genRest n []
    pure $ take n $ monsterPrefix ++ masonsPrefix ++ rest
  where
    -- | Helper function for including a certain set of roles
    -- based on a percentage chance.
    includeIf :: MonadRandom m => Double -> [Role] -> [Role]
    includeIf p rs = percentage p >>= \case
      True -> rs
      False -> []
    -- | The actual generator used by `genRest`.
    gen :: MonadRandom m => [Role] -> m [Role]
    gen rs = generateRole
      [ (Mason/=) -- we generate masons through a separate method
      , (Monster/=) -- the same goes for monsters
      , villagerRole
      , avoidsOverlap rs
      ]
    -- | A generator used to generate `n` villagers.
    genRest :: MonadRandom m => Int -> [Role] -> m [Role]
    genRest 0 rs = pure rs
    genRest n rs = do
      r <- gen rs
      let rs' = r:rs
      genRest (n-1) rs'

-- | A data structure for holding information about
-- the game configuration.
data GameConfig = GameConfig
  { werewolfTeamPercentage :: Double
  -- ^ What percentage of the players should be on the werewolf team.
  , monsterChance :: Double
  -- ^ What chance is there of having a monster in the game?
  , masonChance :: Double
  , masonCount :: Int
  , drunkChance :: Double
  }

-- | Holds a role and modifiers.
data ModRole = ModRole [Modifier] Role

-- | Convert a `Role` to a `ModRole`.
toModRole :: Role -> ModRole
toModRole r = ModRole [] r

-- | Add a modifier to a `ModRole`.
addMod :: Modifier -> ModRole -> ModRole
addMod m (ModRole ms r) = ModRole (m:ms) r

-- | Generate roles for `n` players.
generateRoles :: MonadRandom m => GameConfig -> Int -> m [ModRole]
generateRoles config@GameConfig{drunkChance, werewolfTeamPercentage} count = do
    let werewolfCount = max (floor $ werewolfTeamPercentage * count) 2
        villagerCount = count - werewolfCount
    werewolfTeam <- convert $ generateWerewolfRoles config (werewolfCount-1)
    villagerTeam <- convert $ generateVillagerRoles config villagerCount
    minionIx <- getRandomR (0, villagerCount - 1)
    let villagersAndMinion = villagerTeam & ix minionIx %~ addMod Minion
    roles <- shuffleM $ werewolfTeam ++ villagersAndMinion
    withDrunks <- sequence $ randomAddDrunks <$> roles
    pure withDrunks
  where
    convert = fmap (fmap toModRole)
    randomAddDrunks :: MonadRandom m => ModRole -> m ModRole
    randomAddDrunks mr = percentage drunkChance >>= \case
      True -> addMod Drunk mr
      False -> mr

-- | Generate roles for a list of player names.
generateRolesFor :: MonadRandom m => GameConfig -> [PlayerName] -> m [(PlayerName, ModRole)]
generateRolesFor config playerNames = do
  roles <- generateRoles config $ length playerNames
  pure $ zip playerNames roles

-- | Initialize a player based on their name and role.
initPlayer :: PlayerName -> ModRole -> Player
initPlayer name (ModRole modifiers role) = Player
  { name = name
  , roleData = initialDataFor role
  , modifiers = modifiers
  , status = Alive
  }

-- | This is intended to be run not in the full monad--since
-- without some way to get a Game, there's no way to have a
-- MonadState Game instance--but just inside a MonadRandom
-- instance.
setupGame :: (MonadRandom m) => GameConfig -> [PlayerName] -> m Game
setupGame config playerNames = do
  roles <- generateRolesFor playerNames
  let players = uncurry initPlayer <$> roles
  pure $ initGame players
