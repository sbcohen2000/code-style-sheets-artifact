module StylishText.Utils where

import StylishText.Types

--------------------------------------------------------------------------------

type Pathed a = (Path, a)

indexedMap :: ((Int, a) -> b) -> [a] -> [b]
indexedMap f xs =
  map f $ zip [0..] xs

subvalues1 :: Pathed a -> Pathed a
subvalues1 (p, x0) =
  (p++[0], x0)

subvalues2 :: Pathed (a, b) -> (Pathed a, Pathed b)
subvalues2 (p, (x0, x1)) =
  ((p++[0], x0), (p++[1], x1))

subvalues3 :: Pathed (a, b, c) -> (Pathed a, Pathed b, Pathed c)
subvalues3 (p, (x0, x1, x2)) =
  ((p++[0], x0), (p++[1], x1), (p++[2], x2))

subvalues4 :: Pathed (a, b, c, d) -> (Pathed a, Pathed b, Pathed c, Pathed d)
subvalues4 (p, (x0, x1, x2, x3)) =
  ((p++[0], x0), (p++[1], x1), (p++[2], x2), (p++[3], x3))

subvalues5 :: Pathed (a, b, c, d, e) -> (Pathed a, Pathed b, Pathed c, Pathed d, Pathed e)
subvalues5 (p, (x0, x1, x2, x3, x4)) =
  ((p++[0], x0), (p++[1], x1), (p++[2], x2), (p++[3], x3), (p++[4], x4))

subvalues6 :: Pathed (a, b, c, d, e, f) -> (Pathed a, Pathed b, Pathed c, Pathed d, Pathed e, Pathed f)
subvalues6 (p, (x0, x1, x2, x3, x4, x5)) =
  ((p++[0], x0), (p++[1], x1), (p++[2], x2), (p++[3], x3), (p++[4], x4), (p++[5], x5))

subvaluesN :: Pathed [a] -> [Pathed a]
subvaluesN (p, as) =
  indexedMap (\(i, a) -> (p ++ replicate i 1 ++ [0], a)) as
