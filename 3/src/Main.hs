{-# LANGUAGE TupleSections #-}

import Data.Array.IArray (Array, bounds, listArray, (!))
import Data.Char (digitToInt, isDigit)
import Data.Maybe (catMaybes, mapMaybe)

(!?) :: Array (Int, Int) Char -> (Int, Int) -> Maybe Char
(!?) arr (y, x) | x < 0 || y < 0 = Nothing
(!?) arr (y, x) | y > fst (snd (bounds arr)) = Nothing
(!?) arr (y, x) | x > snd (snd (bounds arr)) = Nothing
(!?) arr (y, x) = Just $ arr ! (y, x)

isPartNumber :: (Int, Int) -> Int -> Array (Int, Int) Char -> Bool
isPartNumber (y, left) right arr =
  any (\c -> c /= '.' && not (isDigit c)) $
    mapMaybe (arr !?) $
      map (y - 1,) [left - 1 .. right + 1] ++ map (y + 1,) [left - 1 .. right + 1] ++ [(y, left - 1), (y, right + 1)]

number :: Int -> (Int, Int) -> Array (Int, Int) Char -> Int
number acc (y, x) arr | x > snd (snd (bounds arr)) = acc
number acc (y, x) arr | isDigit $ arr ! (y, x) = number (acc * 10 + digitToInt (arr ! (y, x))) (y, x + 1) arr
number acc (_, x) _ = acc

number' :: (Int, Int) -> Array (Int, Int) Char -> Int
number' (y, -1) arr = number 0 (y, 0) arr
number' (y, x) arr | isDigit $ arr ! (y, x) = number' (y, x - 1) arr
number' (y, x) arr = number 0 (y, x + 1) arr

main0 :: IO ()
main0 = do
  input <- getContents
  let width = length $ takeWhile ('\n' /=) input
      list = filter ('\n' /=) input
      height = length list `div` width
  print $ solve 0 (0, 0) $ listArray ((0, 0), (height - 1, width - 1)) list
  where
    solve :: Int -> (Int, Int) -> Array (Int, Int) Char -> Int
    solve acc (y, x) arr | y > fst (snd (bounds arr)) = acc
    solve acc (y, x) arr | x > snd (snd (bounds arr)) = solve acc (y + 1, 0) arr
    solve acc (y, x) arr | not $ isDigit $ arr ! (y, x) = solve acc (y, x + 1) arr
    solve acc (y, x) arr =
      let n = number 0 (y, x) arr
          right = x + floor (logBase 10 (fromIntegral n))
       in if isPartNumber (y, x) right arr
            then solve (acc + n) (y, right + 1) arr
            else solve acc (y, right + 1) arr

main :: IO ()
main = do
  input <- getContents
  let width = length $ takeWhile ('\n' /=) input
      list = filter ('\n' /=) input
      height = length list `div` width
  print $ solve 0 (0, 0) $ listArray ((0, 0), (height - 1, width - 1)) list
  where
    solve :: Int -> (Int, Int) -> Array (Int, Int) Char -> Int
    solve acc (y, x) arr | y > fst (snd (bounds arr)) = acc
    solve acc (y, x) arr | x > snd (snd (bounds arr)) = solve acc (y + 1, 0) arr
    solve acc (y, x) arr | arr ! (y, x) /= '*' = solve acc (y, x + 1) arr
    solve acc (y, x) arr =
      let l =
            catMaybes
              [ if maybe False isDigit (arr !? (y, x - 1)) then Just $ number' (y, x - 1) arr else Nothing,
                if maybe False isDigit (arr !? (y, x + 1)) then Just $ number' (y, x + 1) arr else Nothing,
                if maybe False isDigit (arr !? (y - 1, x - 1)) then Just $ number' (y - 1, x - 1) arr else Nothing,
                if maybe True (not . isDigit) (arr !? (y - 1, x - 1)) && maybe False isDigit (arr !? (y - 1, x)) then Just $ number' (y - 1, x) arr else Nothing,
                if maybe True (not . isDigit) (arr !? (y - 1, x)) && maybe False isDigit (arr !? (y - 1, x + 1)) then Just $ number' (y - 1, x + 1) arr else Nothing,
                if maybe False isDigit (arr !? (y + 1, x - 1)) then Just $ number' (y + 1, x - 1) arr else Nothing,
                if maybe True (not . isDigit) (arr !? (y + 1, x - 1)) && maybe False isDigit (arr !? (y + 1, x)) then Just $ number' (y + 1, x) arr else Nothing,
                if maybe True (not . isDigit) (arr !? (y + 1, x)) && maybe False isDigit (arr !? (y + 1, x + 1)) then Just $ number' (y + 1, x + 1) arr else Nothing
              ]
          acc' = if length l == 2 then acc + product l else acc
       in solve acc' (y, x + 1) arr
