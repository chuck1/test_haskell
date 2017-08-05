
module Lists (
  shiftLeft
  ) where

shiftLeft x xs = drop x $ take (x + length xs) (cycle xs)


