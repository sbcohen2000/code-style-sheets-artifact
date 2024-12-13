gcd a b =
  case (a, b) of
    (0, b) -> b
    (a, 0) -> a
    (a, b) -> gcd b (mod a b)

main = print (gcd 270 192)
