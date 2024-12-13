print v = print_ v

r = 100

inCircle x y r =
  x*x + y*y < r*r

calcPi iters = do
  (circ, sqr) <- go 0 0 iters
  pure (div (4 * circ) sqr)

go circ sqr i =
  if i == 0 then pure (circ, sqr)
  else do
    x <- randomRIO (-r, r)
    y <- randomRIO (-r, r)
    if inCircle x y r
      then go (circ+1) (sqr+1) (i-1)
      else go circ (sqr+1) (i-1)

main = do
  pi <- calcPi 1000
  print pi
