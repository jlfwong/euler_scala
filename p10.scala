import scala.collection.mutable.Map

def sieve(max: Long): List[Long] = {
  val isPrime = Map[Long, Boolean]()
  2L :: (3L to max by 2L filter { i =>
    if (isPrime.getOrElse(i, true)) {
      i to max by i foreach { isPrime(_) = false }
      true
    } else false
  } toList)
}

def a1(max: Long): Long = sieve(max).sum

println(a1(10))
println
println(a1(2000000))
