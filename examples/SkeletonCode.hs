module Main where

-- myZip takes two lists and returns
-- a single list where corresponding elements
-- have been grouped into a tuple.
--
-- If one list is longer than the other, remaining
-- elements are discarded.
myZip :: [a] -> [b] -> [(a, b)]
myZip = {- your code here -} undefined

main = do
  let input1 = [1, 2, 3]
      input2 = [True, False, True]
  print $ myZip input1 input2
