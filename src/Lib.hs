module Lib
  ( main'
  ) where

-- v1 : Break the problem into two parts: parsing input (rankings)
--      and calculating output (results).

-- data ----------------------------------------------------

-- Ranking information.
data Ranking = Ranking
  {
  } deriving Show

-- Result information.
data Result = Result
  {
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

-- Program Entry
main' :: IO ()
main' = undefined
