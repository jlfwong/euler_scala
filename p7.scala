def primes(): Iterator[Int] = {
  var primes: List[Int] = Nil
  var next = 3
  Iterator(2) ++ Iterator.continually {
    while (primes.exists(next % _ == 0)) next += 2
    primes = next :: primes
    next
  }
}

def a1(n: Int): Int = primes.take(n).toList.last

println(a1(6))
println
println(a1(10001))
