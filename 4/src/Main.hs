{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Data.Array (elems)
import Data.Array.MArray (MArray (newArray), readArray, writeArray)
import Data.Array.ST (runSTArray)
import Data.Char (isDigit)
import Data.Set qualified as Set
import Data.Text (pack, replace, splitOn, unpack)

main0 :: IO ()
main0 = do
  input <- getContents
  let f = map read . filter (not . null) . map unpack . splitOn " "
      lines :: [(Set.Set Int, [Int])]
      lines = map ((\l -> (Set.fromList $ f $ head l, f $ head $ tail l)) . splitOn "|" . head . tail . splitOn ":") (splitOn "\n" $ pack input)
      result = sum $ map (\(s, l) -> let c = length (filter (`Set.member` s) l) in if c == 0 then 0 else 2 ^ (c - 1)) lines
  print result

main :: IO ()
main = do
  input <- getContents
  let f = map read . filter (not . null) . map unpack . splitOn " "
      lines :: [(Set.Set Int, [Int])]
      lines = map ((\l -> (Set.fromList $ f $ head l, f $ head $ tail l)) . splitOn "|" . head . tail . splitOn ":") (splitOn "\n" $ pack input)
      array = runSTArray $ do
        a <- newArray (0, length lines - 1) 1
        forM_ (zip lines [0 ..]) $ \((s, l), i) -> do
          let c = length (filter (`Set.member` s) l)
          s <- readArray a i
          forM_ [i + 1 .. i + c] $ \j -> do
            x <- readArray a j
            writeArray a j $ x + s
        return a
  print $ sum $ elems array
