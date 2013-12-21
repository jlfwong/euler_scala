import Math.sqrt
import scala.collection.immutable.HashSet

def properDivisors(num: Int) =
  1 :: (2 to sqrt(num).toInt).foldLeft(List[Int]()) { (res: List[Int], i: Int) =>
    if (num % i == 0) i :: num / i :: res
    else res
  }

def amicableChainLength(starting: Int, max: Int) = {
  def f(n: Int, length: Int, visited: HashSet[Int]): Int = {
    val next = properDivisors(n).sum
    if (next == starting) length
    else if (next > max || next < starting || visited.contains(next)) -1
    else f(next, length + 1, visited + next)
  }

  f(starting, 1, HashSet(starting))
}

def a1(max: Int) = {
  (1 to max) map{n: Int => (n, amicableChainLength(n, max))} maxBy {_._2}
}

println(a1(300))
println(a1(1000000))

