import Math.floor
import Math.sqrt

def divisors(num: Int) =
  (1 to sqrt(num).toInt).foldLeft(List[Int]()) { (res, i) =>
    if (num % i == 0) i :: num / i :: res
    else res
  }

def triangular(num: Int) = num * (num - 1) / 2

def a1(count: Int) =
  Stream.from(1).map{triangular _}.find{divisors(_).length > count}.get

println(a1(5))
println(a1(50))
println(a1(100))
println(a1(500))
