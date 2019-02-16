module Main where

import           RegexSimple
import           RegexWeighted

main :: IO ()
main = do
  let as = Alt (Sym 'a') (Rep (Sym 'b'))
  let result = acceptW (weighted as) "a" :: Int
  print result
