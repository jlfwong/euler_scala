import scala.collection.immutable.BitSet

lazy val primes: Stream[Int] = 2 #:: Stream.from(3, 2).filter { num: Int =>
  primes takeWhile{n: Int => n * n <= num} forall{num % _ != 0}
}

lazy val fourDigitPrimes = primes.takeWhile(_ <= 9999).filter(_ >= 1000)

lazy val evenSpacedFourDigitPrimeTriples: Seq[Tuple3[Int, Int, Int]] = {
   val fourDigitPrimeSet = BitSet(fourDigitPrimes: _*)
   for {
    a <- (0 until fourDigitPrimes.length)
    b <- (a+1 until fourDigitPrimes.length)
    pA = fourDigitPrimes(a)
    pB = fourDigitPrimes(b)
    pC = pB + (pB - pA)
    if fourDigitPrimeSet(pC)
  } yield (pA, pB, pC)
}

def isPerm(a: Int, b: Int) =
  a.toString.sorted == b.toString.sorted

lazy val a1 = {
  evenSpacedFourDigitPrimeTriples.filter {
    case (pA: Int, pB: Int, pC: Int) => isPerm(pA, pB) && isPerm(pB, pC)
  }
}

println(a1)
