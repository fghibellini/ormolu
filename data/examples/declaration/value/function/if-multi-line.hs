foo :: Int -> Int
foo x = if x > 5 then 10
  else 12

bar :: Int -> Int
bar x =
  if x > 5
  then foo x
         + 100
  else foo x
         + 200
