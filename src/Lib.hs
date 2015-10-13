module Lib
  ( main'
  ) where

-- v8 : Functions for averaging ranks and producing their standard deviation.
--      Produce results and wire things up from rankings to results.

import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe
import Data.Ord
import Text.Parsec
import Text.Parsec.String

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

-- Map teams to their divisions
teamDivisionMap :: Map.Map String String
teamDivisionMap = undefined

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

-- Program Entry
main' :: IO ()
main' = undefined
