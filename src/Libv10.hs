module Libv10
  ( main'
  ) where

-- v10 : Fill in the division team mapping and other data

import Control.Monad
import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe
import Data.Ord
import System.Environment
import Text.Parsec
import Text.Parsec.String
import Text.Printf

-- data ----------------------------------------------------

-- Ranking information - team rank, team name
data Ranking = Ranking
  { rRank :: Int
  , rTeam :: String
  } deriving Show

-- Result information - division, average, standard deviation
data Result = Result
  { rDivision :: String
  , rAverage  :: Double
  , rStdDev   :: Double
  } deriving Show

-- parsing -------------------------------------------------

-- Parse out a single ranking from the file
rankingParser :: Parser Ranking
rankingParser = do
  many space
  rank <- many1 digit
  many space
  team <- many1 alphaNum
  newline
  return (Ranking (read rank) team)

-- Parse out all of the rankings
rankingsParser :: Parser [Ranking]
rankingsParser = many1 rankingParser

-- Take a file and produce a list of team rankings
parseRankings :: String -> IO [Ranking]
parseRankings file =
  parseFromFile rankingsParser file >>= either undefined return

-- calculating ---------------------------------------------

-- >> AFC divisions
afcEast, afcWest, afcSouth, afcNorth :: String
afcEast = "AFC East"
afcWest = "AFC West"
afcSouth = "AFC South"
afcNorth = "AFC North"

-- >> NFC divisions
nfcEast, nfcWest, nfcSouth, nfcNorth :: String
nfcEast = "NFC East"
nfcWest = "NFC West"
nfcSouth = "NFC South"
nfcNorth = "NFC North"

-- >> Map teams to their divisions
teamDivisionMap :: Map.Map String String
teamDivisionMap = Map.fromList
  [ ("PATRIOTS",   afcEast)
  , ("JETS",       afcEast)
  , ("DOLPHINS",   afcEast)
  , ("BILLS",      afcEast)

  , ("BENGALS",    afcNorth)
  , ("BROWNS",     afcNorth)
  , ("STEELERS",   afcNorth)
  , ("RAVENS",     afcNorth)

  , ("BRONCOS",    afcWest)
  , ("RAIDERS",    afcWest)
  , ("CHARGERS",   afcWest)
  , ("CHIEFS",     afcWest)

  , ("JAGUARS",    afcSouth)
  , ("TITANS",     afcSouth)
  , ("TEXANS",     afcSouth)
  , ("COLTS",      afcSouth)

  , ("FALCONS",    nfcSouth)
  , ("PANTHERS",   nfcSouth)
  , ("BUCCANEERS", nfcSouth)
  , ("SAINTS",     nfcSouth)

  , ("PACKERS",    nfcNorth)
  , ("VIKINGS",    nfcNorth)
  , ("LIONS",      nfcNorth)
  , ("BEARS",      nfcNorth)

  , ("COWBOYS",    nfcEast)
  , ("REDSKINS",   nfcEast)
  , ("GIANTS",     nfcEast)
  , ("EAGLES",     nfcEast)

  , ("CARDINALS",  nfcWest)
  , ("RAMS",       nfcWest)
  , ("49ERS",      nfcWest)
  , ("SEAHAWKS",   nfcWest)
  ]


-- Lookup a team's division - assume well-formed data files :o
teamToDivision :: String -> String
teamToDivision team =
  fromJust (Map.lookup team teamDivisionMap)

-- Convert a ranking to a (division, rank) tuple
rankingToDivision :: Ranking -> (String, Int)
rankingToDivision ranking =
  (teamToDivision (rTeam ranking), rRank ranking)

-- Group sort - collect values with identical keys
groupSort :: Ord k => [(k, v)] -> [(k, [v])]
groupSort kvs = Map.toList (Map.fromListWith (++) [(k, [v]) | (k, v) <- kvs])

-- Turn the rankings into a (division, [rank]) tuple
rankingsToDivisions :: [Ranking] -> [(String, [Int])]
rankingsToDivisions rankings =
  groupSort (map rankingToDivision rankings)

-- Average a number of rankings
average :: [Int] -> Double
average rankings = realToFrac (sum rankings) / genericLength rankings

-- Standard deviation of rankings group
stdDev :: [Int] -> Double
stdDev rankings = sqrt (sum (map f rankings) / genericLength rankings) where
  f ranking = (realToFrac ranking - average rankings) ** 2

-- Produce a result from division and [rank]
calculateResult :: (String, [Int]) -> Result
calculateResult (division, rankings) =
  Result division (average rankings) (stdDev rankings)

-- Take teams rankings and calculate results
calculateResults :: [Ranking] -> [Result]
calculateResults rankings =
  sortBy (comparing rAverage) (map calculateResult (rankingsToDivisions rankings))

-- main ----------------------------------------------------

-- Combine the parsing and calculating
parseAndCalculate :: String -> IO [Result]
parseAndCalculate file = do
  rankings <- parseRankings file
  return (calculateResults rankings)

-- Collect results from all the files
parseAndCalculateAll :: [String] -> IO [(String, [Result])]
parseAndCalculateAll files =
  forM files $ \file -> do
    results <- parseAndCalculate file
    return (file, results)

-- Print out a result
printResult :: Result -> IO ()
printResult result =
  printf "%-10s %10.2f %10.2f\n" (rDivision result) (rAverage result) (rStdDev result)

-- Print out all the results
inputsToOutputs :: [(String, [Result])] -> IO ()
inputsToOutputs results =
  forM_ results $ \(file, results') -> do
    putStrLn file
    forM_ results' printResult
    putStrLn ""

-- Program Entry - pass file args as inputs
main' :: IO ()
main' = do
  args <- getArgs
  results <- parseAndCalculateAll args
  inputsToOutputs results


