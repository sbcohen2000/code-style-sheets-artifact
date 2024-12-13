f . g = \a -> f (g a)

f >>> g = \a -> g (f a)

not b = if b then 0 == 1 else 0 == 0

f >>= a = do
  r <- f
  a r

print v = print_ v

getContents = getContents_ 0

lines s = lines_ s

isPrefixOf pfx = \s -> isPrefixOf_ pfx s

filter f = \as ->
  case as of
    (a:rst) ->
      if f a then a:(filter f) rst else (filter f) rst
    [] -> []

length as =
  case as of
    (_:rst) -> 1 + length rst
    [] -> 0

main =
  getContents
    >>= print
      . length
      . (\ls -> trace ((filter (isPrefixOf "--")) ls))
      . lines
