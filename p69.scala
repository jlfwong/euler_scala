import scala.collection.immutable.BitSet

lazy val primes: Stream[Int] = 2 #:: Stream.from(3, 2).filter { num: Int =>
  primes takeWhile{n: Int => n * n <= num} forall{num % _ != 0}
}

def primeFactors(num: Int) =
  primes takeWhile{_ <= num} filter{num % _ == 0}

def phi(num: Int) = {
  (num - 1) - (primeFactors(num) map{n: Int => BitSet((n until num by n).toList: _*)}).foldLeft(BitSet()){
      (res: BitSet, s: BitSet) => res ++ s
  }.size
}

def phi2(num: Int) = {
  val factors = primeFactors(num)
  (1 to num - 1).filter{ n: Int => !factors.exists(n % _ == 0) }.length
}

def a1(max: Int) =
  (2 to max) map{n: Int => (n, n.toDouble / phi2(n))} maxBy{_._2}

def a2(max: Int) = {
  var curPrimes = primes
  var num = 1
  while (num * curPrimes.head < max) {
    num *= curPrimes.head
    curPrimes = curPrimes.tail
  }
  (num, num.toDouble / phi2(num))
}

println(a1(10))
println(a2(10))
println(a1(10000))  // Too slow!
println(a2(1000000))
