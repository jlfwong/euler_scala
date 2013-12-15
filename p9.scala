def a1(): Int =
  (for {
    a <- (1 until 1000)
    b <- (a + 1 until 1000)
    c = 1000 - a - b
    if a*a + b*b == c*c
  } yield a * b * c).toList(0)

println(a1)
