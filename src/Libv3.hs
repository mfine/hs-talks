module Libv3
  ( main'
  ) where

-- v3 : Setup the parsing of the input

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

-- >> Parse out a single ranking from the file
rankingParser :: Parser Ranking
rankingParser = undefined

-- >> Parse out all of the rankings
rankingsParser :: Parser [Ranking]
rankingsParser = undefined

-- Take a file and produce a list of team rankings
parseRankings :: String -> IO [Ranking]
parseRankings = undefined

-- calculating ---------------------------------------------

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
