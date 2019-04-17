{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Control.Foldl              as L
import           Control.Monad
import           Control.Monad.Random.Class
import           Data.Foldable
import           Data.Maybe                 (fromJust)
import           System.Random.Shuffle

main :: IO ()
main =
  do
     putStrLn "After playing 100 games:"
     games <- playRandomGames
     print $ summarise games


playRandomGame :: MonadRandom m => m Int
playRandomGame =
  do
    let gamePieces = splitUpTiles totalTiles
    randomGame <- shuffleM gamePieces
    let tiles = playGame randomGame
    pure $ length tiles

playRandomGames :: MonadRandom m => m [Int]
playRandomGames = replicateM 100 playRandomGame


type Board = [Tile]
type GameState = (Tile, Board)


data Tile = Tile Int
  deriving Show


data TileProb = TileProb
  { p1 :: Double
  , p2 :: Double
  , p3 :: Double
  , p4 :: Double
  }

probs :: TileProb
probs = TileProb{..}
  where
    p1    = 0.5
    p2    = (1 - p1) / 2
    p1p2  = p1 + p2
    p1p2' = 1 - p1p2
    p3    = p1p2' * 0.8
    p4    = p1p2' * 0.2

playGame :: Board -> [Tile]
playGame gameTurns = reverse $ go gameTurns []
  where
    go :: Board -> [Tile] -> [Tile]
    go [] ts       = ts
    go (t@(Tile n) : bs) ts =
      let
        newBoard = drop (n - 1) bs
      in
        go newBoard (t : ts)


totalTiles :: Int
totalTiles = 78

splitUpTiles :: Int -> [Tile]
splitUpTiles len = fold [ts1, ts2, ts3, ts4]
  where
    len' = fromIntegral len
    t4 = floor $ len' * (p4 probs)
    t3 = floor $ len' * (p3 probs)
    t2 = floor $ len' * (p2 probs)
    t1 = len - (t2 + t3 + t4)

    ts1 = take t1 $ repeat (Tile 1)
    ts2 = take t2 $ repeat (Tile 2)
    ts3 = take t3 $ repeat (Tile 3)
    ts4 = take t4 $ repeat (Tile 4)



data SummaryStat = SummaryStat
  { meanS              :: Double
  , standardDeviationS :: Double
  , minS               :: Maybe Int
  , maxS               :: Maybe Int
  }

instance Show SummaryStat where
  show SummaryStat{..} =
    unlines
      [ "The average length of a game was:                 " <> show meanS
      , "The standard deviation in the length of the game: " <> show standardDeviationS
      , "The minimum length of all games was:              " <> show (fromJust minS)
      , "The maximum length of all games was:              " <> show (fromJust maxS)
      ]

summarise :: [Int] -> SummaryStat
summarise xs = SummaryStat{..}
  where
    (meanS, standardDeviationS, minS, maxS)
      = L.fold summaryFold xs

    summaryFold = (,,,) <$> mean' <*> std' <*> L.minimum <*> L.maximum


mean' :: Fractional a =>  L.Fold Int a
mean' = L.premap fromIntegral L.mean

std' :: Floating a =>  L.Fold Int a
std' = L.premap fromIntegral L.std




