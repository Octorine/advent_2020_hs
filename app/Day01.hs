
module Day01 where
import Game.Advent
import Data.List
import Paths_advent2022_hs (getDataFileName)

{- | Part 1.  Input contains a list of lists of numbers, with blank
     lines in between.  Output is the largest total from the lists.

>>> d01p1 "day01-ex.txt"
"24000"
-}
d01p1 filename = do

  totals <- map (sum . (read<$>)) . split . lines <$> (readFile =<< getDataFileName filename)
  return  . show . maximum $ totals


{-| Split a string into substrings delimited by empty strings

>>> split ["1", "2", "3", "", "4", "5", "6"]
[["1","2","3"],["4","5","6"]]
-}
split :: [String] -> [[String]]
split [] = []
split ss = takeWhile (not . null) ss : split (dropWhile null (dropWhile (not . null) ss))


{- | Part 2.  Part 2.  Input contains a list of lists of numbers, with blank
     lines in between.  Output is sum of the three largest totals from
     the lists.
>>> d01p2 "day01-ex.txt"
"45000"
-}
d01p2 filename = do
  totals <- map (sum . (read<$>))
            . split
            . lines
            <$> (readFile =<< getDataFileName filename)
  return  . show . sum . take 3 . reverse . sort $ totals


day01 :: Day
day01 =
  Day
    { dayName = "01",
      dayPart1 = d01p1 "day01.txt",
      dayPart2 = d01p2 "day01.txt"
    }
