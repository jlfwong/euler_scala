def a1(max: Int): Int =
  (for (x <- 1 until max if x % 3 == 0 || x % 5 == 0) yield x).sum

def a2(max: Int): Int =
  (1 until max).filter(x => x % 3 == 0 || x % 5 == 0).sum

println(a1(10))
println(a2(10))
println
println(a1(1000))
println(a2(1000))
