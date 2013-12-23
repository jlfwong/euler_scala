def primes(): Iterator[Int] = {
  var primes: List[Int] = Nil
  var next = 3
  Iterator(2) ++ Iterator.continually {
    while (primes.exists(next % _ == 0)) next += 2
    primes = primes ++ List(next)
    next
  }
}

def primes2(): Iterator[Int] = {
  var primes: List[Int] = Nil
  Iterator(2) ++ Stream.from(3, 2).filter{ n =>
    if (primes.exists(n % _ == 0)) false
    else {
      primes = primes ++ List(n)
      true
    }
  }
}

lazy val primes3: Stream[Int] = 2 #:: Stream.from(3, 2).filter { num: Int =>
  primes3 takeWhile{n: Int => n * n <= num} forall{num % _ != 0}
}

def a1(n: Int): Int = primes.take(n).toList.last
def a2(n: Int): Int = primes2.take(n).toList.last
def a3(n: Int): Int = primes3.take(n).toList.last

println(a1(6))
println(a2(6))
println(a3(6))
println
println(a1(10001))
println(a2(10001))
println(a3(10001))
