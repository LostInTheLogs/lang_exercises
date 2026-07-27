doubleMe x = x + x

doubleSmallNumber x =
  if x <= 100
    then x * 2
    else x

rightTriangles = [(a, b, c) | c <- [1 .. 10], a <- [1 .. c], b <- [1 .. a], a ^ 2 + b ^ 2 == c ^ 2]
