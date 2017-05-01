module Exercises2 where


data Weekday = Mon|Tue|Wed|Thur|Fri|Sat|Sun
  deriving ({-Ord, -}Show, Enum)

instance Eq Weekday where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thur Thur = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

{-now do things like 'prev Sat' because we derived from Enum-}

instance Ord Weekday where
  {-this is bullshit-}
  {-it is hard to cover all the cases-}
  (<=) Mon Tue = True
  (<=) Tue Wed = True
  (<=) Wed Thur = True
  (<=) Thur Fri = True
  (<=) Fri Sat = True
  (<=) Sat Sun = True
  {-or we should implement compare -}
  {-but there are many cases too-}
