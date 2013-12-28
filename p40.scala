// The position in the fractional part of 10^k
def pos10(k: Int) = ((1 to k) map{a: Int => (k - a + 1) * 9 * BigInt(10).pow(k - a)}).sum

val pos10s = 0 #:: Stream.from(1) map{pos10}

def numAtPos(n: Int): Tuple2[BigInt, BigInt] = {
  if (n == 0) (BigInt(1), BigInt(0))
  else {
    val k = pos10s.indexWhere{_ >= n} - 1
    (BigInt(10).pow(k) + (n - pos10s(k)) / (k + 1), (n - pos10s(k)) % (k + 1))
  }
}

def d(n: Int) = {
  val (num, digPos) = numAtPos(n-1)
  (num.toString.sliding(1).toList)(digPos.toInt).toInt
}

val a1 = d(1) * d(10) * d(100) * d(1000) * d(10000) * d(100000) * d(1000000)

println(a1)