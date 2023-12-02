{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isDigit)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Text.Parsec (Parsec, char, parse, sepBy1)
import Text.Parsec.Char (digit, string)
import Text.Parsec.Combinator (many1, sepBy)

data Game = Game
  { number :: Int,
    sets :: [(Int, Int, Int)]
  }

parser :: Parsec String () Game
parser = do
  let num = read <$> many1 digit
      red = "red"
      green = "green"
      blue = "blue"
      color = string red <|> string green <|> string blue
      f color = fst . fromMaybe (0, color) . find (\(_, c) -> c == color)
      set = fmap (\l -> (f red l, f green l, f blue l)) $ ((,) <$> (char ' ' *> num) <*> (char ' ' *> color)) `sepBy1` string ","
  string "Game "
  number <- num
  char ':'
  sets <- set `sepBy1` string ";"
  return Game {number, sets}

isPossible :: (Int, Int, Int) -> Bool
isPossible (r, g, b) = r <= 12 && g <= 13 && b <= 14

maxSet :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
maxSet (r1, g1, b1) (r2, g2, b2) = (max r1 r2, max g1 g2, max b1 b2)

main0 :: IO ()
main0 =
  getContents
    >>= print
      . sum
      . map number
      . filter (all isPossible . sets)
      . either (error . show) id
      . parse (parser `sepBy` string "\n") ""

main :: IO ()
main =
  getContents
    >>= print
      . sum
      . map ((\(r, g, b) -> r * g * b) . foldl1 maxSet . sets)
      . either (error . show) id
      . parse (parser `sepBy` string "\n") ""
