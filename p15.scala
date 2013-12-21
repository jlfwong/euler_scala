def a1(dim: Int): Long = {
  val dyn = Array.fill(dim + 1, dim + 1)(0L)
  for {
    y <- 0 to dim
    x <- 0 to dim
  } {
    dyn(y)(x) = if (x == 0 || y == 0) 1L
    else dyn(y-1)(x) + dyn(y)(x-1)
  }
  dyn(dim)(dim)
}

println(a1(2))
println(a1(20))