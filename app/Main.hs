module Main where
import Game.Advent
import Day01 (day01)
import Day02 (day02)
main :: IO ()
main = do
  runDay day01
  runDay day02
  
