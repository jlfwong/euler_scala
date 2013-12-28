lazy val primes: Stream[Int] = 2 #:: Stream.from(3, 2).filter { num: Int =>
  primes takeWhile{n: Int => n * n <= num} forall{num % _ != 0}
}

def isPrime(n: Int) = primes.takeWhile(_ <= n).lastOption.getOrElse(-1) == n

def breaksGoldbach(n: Int) =
  if (isPrime(n)) false
  else !(Stream.from(0) map{i: Int => 2 * i * i} takeWhile{_ < n} exists{twoI2: Int => isPrime(n - twoI2)})

lazy val a1 = Stream.from(3, 2).find(breaksGoldbach _).get

println(a1)

