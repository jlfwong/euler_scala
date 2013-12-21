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

val factorial: (Int => BigInt) = 1L #:: Stream.from(1).map {x: Int => x * factorial(x - 1)}
def nCr(n: Int, k: Int): BigInt = factorial(n) / (factorial(k) * factorial(n - k))
def a2(dim: Int): BigInt = nCr(2*dim, dim)

println(a1(2))
println(a2(2))
println(a1(20))
println(a2(20))
