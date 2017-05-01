data Weekday = Mon|Tue|Wed|Thur|Fri|Sat|Sun

data Date = Date Weekday Int

instance Eq Weekday where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thur Thur = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date wd dateOfMonth)
       (Date od oDateOfMOnth) =
      wd == od && dateOfMonth == oDateOfMOnth

f :: Int -> Bool
f 1 = True
f 2 = True
f 3 = True
f 4 = True
f 5 = True
{-if you do not have this case then enabling :set -Wall will show warning-}
f _ = False

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'
