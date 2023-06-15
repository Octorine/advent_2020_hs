module Day02 where
import Game.Advent
import Paths_advent2022_hs (getDataFileName)

{- | Part 1.  Short description of the problem.
>>> d02p1 "day02-ex.txt"
"TODO"
-}
d02p1 filename = do
  code <- map words . lines <$> (readFile =<< getDataFileName filename)
  return "TODO"

{- | Part 2.  Short description of the problem.
>>> d02p2 "day02-ex.txt"
"TODO"
-}
d02p2 filename = do
  code <- map words . lines <$> (readFile =<< getDataFileName filename)
  return "TODO"


day02 :: Day
day02 =
  Day { dayName = "02",
        dayPart1 = d02p1 "day02.txt",
        dayPart2 = d02p2 "day02.txt"
    }
