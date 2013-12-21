def a1(pow: Int) = {
  lazy val powersOf2: (Int => BigInt) = 1 #:: Stream.from(1).map{n: Int => BigInt(2) * powersOf2(n-1)}
  powersOf2(pow).toString.sliding(1).map(_.toInt).sum
}

println(a1(15))
println(a1(1000))
