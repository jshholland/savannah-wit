module Util where

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy c s = case rest of
                [] -> [chunk]
                _:rest' -> chunk : splitBy c rest'
  where (chunk, rest) = break (== c) s
