import scala.collection.mutable.BitSet

def sieve(max: Int): List[Int] = {
  val composite = BitSet()
  2 :: (3 to max by 2 filter { i =>
    if (composite(i)) false
    else {
      i to max by i foreach { composite += _ }
      true
    }
  }).toList
}

def a1(max: Int): Long = sieve(max).map(_.toLong).sum

println(a1(10))
println
println(a1(2000000))
