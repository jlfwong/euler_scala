import Math.sqrt

def properDivisors(num: Int) =
  1 :: (2 to sqrt(num).toInt).foldLeft(List[Int]()) { (res: List[Int], i: Int) =>
    if (num % i == 0) i :: num / i :: res
    else res
  }

def a1(max: Int): Int =
  ((1 until max) map { a: Int =>
    val b = properDivisors(a).sum
    if (b > a && properDivisors(b).sum == a) a + b
    else 0
  }).sum

println(a1(10000))

