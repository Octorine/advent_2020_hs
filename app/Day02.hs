{-# LANGUAGE NamedFieldPuns #-}

module Day02 where

import Data.Char (ord)
import Data.Maybe
import Game.Advent
import Paths_advent2022_hs (getDataFileName)

-- | Part 1.  Short description of the problem.
-- >>> d02p1 "day02-ex.txt"
-- "15"
d02p1 filename = do
  rounds <- map parseRoundP1 . lines <$> (readFile =<< getDataFileName filename)
  return . show . sum . map score $ rounds

-- | Part 2.  Short description of the problem.
-- >>> d02p2 "day02-ex.txt"
-- "12"
d02p2 filename = do
  rounds <- map parseRoundP2 . lines <$> (readFile =<< getDataFileName filename)
  return . show . sum . map score $ rounds

day02 :: Day
day02 =
  Day
    { dayName = "02",
      dayPart1 = d02p1 "day02.txt",
      dayPart2 = d02p2 "day02.txt"
    }

score :: Round -> Int
score (Round player opponent outcome) =
  outcomePoints outcome + playPoints player

data Round = Round
  { player :: Play,
    opponent :: Play,
    outcome :: Outcome
  }
  deriving (Show)

data Play = Rock | Paper | Scissors
  deriving (Show, Eq)

playPoints Rock = 1
playPoints Paper = 2
playPoints Scissors = 3

parseOpponent "A" = Rock
parseOpponent "B" = Paper
parseOpponent "C" = Scissors

data Outcome = Win | Draw | Lose
  deriving (Show, Eq)

outcomePoints Win = 6
outcomePoints Draw = 3
outcomePoints Lose = 0

parsePlayer "X" = Rock
parsePlayer "Y" = Paper
parsePlayer "Z" = Scissors

parseOutcome "X" = Lose
parseOutcome "Y" = Draw
parseOutcome "Z" = Win

getOutcome :: Play -> Play -> Outcome
getOutcome Scissors Paper = Win
getOutcome Paper Rock = Win
getOutcome Rock Scissors = Win
getOutcome a b | a == b = Draw
getOutcome _ _ = Lose

getPlayer :: Play -> Outcome -> Play
getPlayer opponent Draw = opponent
getPlayer Paper Win = Scissors
getPlayer Paper Lose = Rock
getPlayer Scissors Win = Rock
getPlayer Scissors Lose = Paper
getPlayer Rock Win = Paper
getPlayer Rock Lose = Scissors

parseRoundP1 :: String -> Round
parseRoundP1 input = Round {player, opponent, outcome}
  where
    [opponentString, playerString] = words input
    opponent = parseOpponent opponentString
    player = parsePlayer playerString
    outcome = getOutcome player opponent

parseRoundP2 :: String -> Round
parseRoundP2 input = Round {opponent, player, outcome}
  where
    [opponentString, outcomeString] = words input
    opponent = parseOpponent opponentString
    outcome = parseOutcome outcomeString
    player = getPlayer opponent outcome
