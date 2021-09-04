module Main
where

import System.Environment
import qualified Data.List as List

import CrazyhouseBot

main :: IO ()
main = do
  args <- getArgs
  let oneString = concat (List.intersperse " " args)
  putStrLn ( getMove oneString )
