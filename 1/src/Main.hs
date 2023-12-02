{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.Text (pack, replace, splitOn, unpack)

main :: IO ()
main =
  getContents
    >>= print
      . sum
      . map (read . (\s -> [head s, last s]) . filter isDigit)
      . filter (not . null)
      . map unpack
      . splitOn "\n"
      . replace "one" "one1one"
      . replace "two" "two2two"
      . replace "three" "three3three"
      . replace "four" "four4four"
      . replace "five" "five5five"
      . replace "six" "six6six"
      . replace "seven" "seven7seven"
      . replace "eight" "eight8eight"
      . replace "nine" "nine9nine"
      . pack
