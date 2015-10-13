module Libv2
  ( main'
  ) where

-- v2 : Link the two parts and define the data to input and output.

-- data ----------------------------------------------------

-- >> Ranking information - team rank, team name
data Ranking = Ranking
  { rRank :: Int
  , rTeam :: String
  } deriving Show

-- >> Result information - division, average, standard deviation
data Result = Result
  { rDivision :: String
  , rAverage  :: Double
  , rStdDev   :: Double
  } deriving Show

-- parsing -------------------------------------------------

-- Take a file and produce a list of team rankings
parseRankings :: String -> IO [Ranking]
parseRankings = undefined

-- calculating ---------------------------------------------

-- Take teams rankings and calculate results
calculateResults :: [Ranking] -> [Result]
calculateResults = undefined

-- main ----------------------------------------------------

-- >> Combine the parsing and calculating
parseAndCalculate :: String -> IO [Result]
parseAndCalculate file = do
  rankings <- parseRankings file
  return (calculateResults rankings)

-- Program Entry
main' :: IO ()
main' = undefined
