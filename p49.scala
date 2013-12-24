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

lazy val evenSpacedFourDigitPrimeTriples2: Seq[Tuple3[Int, Int, Int]] = {
  val fourDigitPrimeSet = BitSet(fourDigitPrimes: _*)
  (0 until fourDigitPrimes.length) flatMap{ a:Int =>
    (a+1 until fourDigitPrimes.length).map {b: Int => (a, b)}
  } map {
    case (a, b) => {
      val pA = fourDigitPrimes(a)
      val pB = fourDigitPrimes(b)
      val pC = pB + (pB - pA)
      (pA, pB, pC)
    }
  } filter {
    case (pA, pB, pC) => fourDigitPrimeSet(pC)
  }
}

def isPerm(a: Int, b: Int) =
  a.toString.sorted == b.toString.sorted

lazy val a1 = {
  evenSpacedFourDigitPrimeTriples.filter {
    case (pA: Int, pB: Int, pC: Int) => isPerm(pA, pB) && isPerm(pB, pC)
  }
}

lazy val a2 = {
  evenSpacedFourDigitPrimeTriples2.filter {
    case (pA: Int, pB: Int, pC: Int) => isPerm(pA, pB) && isPerm(pB, pC)
  }
}

println(a1)
println(a2)
