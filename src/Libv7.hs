module Libv7
  ( main'
  ) where

-- v7 : Helper functions to group together divisions

import qualified Data.Map.Strict as Map
import Data.Maybe
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

-- >> Convert a ranking to a (division, rank) tuple
rankingToDivision :: Ranking -> (String, Int)
rankingToDivision ranking =
  (teamToDivision (rTeam ranking), rRank ranking)

-- >> Group sort - collect values with identical keys
groupSort :: Ord k => [(k, v)] -> [(k, [v])]
groupSort kvs = Map.toList (Map.fromListWith (++) [(k, [v]) | (k, v) <- kvs])

-- >> Turn the rankings into a (division, [rank]) tuple
rankingsToDivisions :: [Ranking] -> [(String, [Int])]
rankingsToDivisions rankings =
  groupSort (map rankingToDivision rankings)

-- Take teams rankings and calculate results
calculateResults :: [Ranking] -> [Result]
calculateResults = undefined

-- main ----------------------------------------------------

-- Combine the parsing and calculating
parseAndCalculate :: String -> IO [Result]
parseAndCalculate file = do
  rankings <- parseRankings file
  return (calculateResults rankings)

-- Program Entry
main' :: IO ()
main' = undefined
