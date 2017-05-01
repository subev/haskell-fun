module Tuples where

import Data.Tuple

ohSeven :: (a) -> (Int, a, Int)
ohSeven x = (0, x, 7)

toBeCurried :: (a,b) -> b
toBeCurried x = snd x

t1 = curry toBeCurried 1 2 {-should return 2-}
t2 = uncurry (+) (1,2) {-should return 3-}

