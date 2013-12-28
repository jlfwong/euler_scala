lazy val primes: Stream[Long] = 2 #:: Stream.from(3, 2).filter { num: Int =>
  primes takeWhile{n: Long => n * n <= num} forall{num % _ != 0}
} map {_.toLong}

def rotations[T](xs: Seq[T]): Iterator[Seq[T]] = {
  (xs ++ xs.init).sliding(xs.length)
}

def a1(max: Long) = {
  val ps = primes takeWhile{_ < max}
  val isPrime = Set(ps: _*)
  (ps filter{n:Long => rotations(n.toString) map{_.mkString.toLong} forall{isPrime(_)}}).length
}

println(a1(100))
println(a1(10000))
println(a1(1000000))
