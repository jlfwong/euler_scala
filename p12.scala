import Math.floor
import Math.sqrt

def divisors(num: Int) =
  (1 to sqrt(num).toInt).foldLeft(List[Int]()) { (res: List[Int], i: Int) =>
    if (num % i == 0) i :: num / i :: res
    else res
  }

def divisors2(num: Int) =
  (List[Int]() /: (1 to sqrt(num).toInt)) { (res: List[Int], i: Int) =>
    if (num % i == 0) i :: num / i :: res
    else res
  }

def triangular(num: Int) = num * (num - 1) / 2

def a1(count: Int) =
  Stream.from(1).map{triangular _}.find{divisors(_).length > count}.get

def a2(count: Int) =
  Stream.from(1).map{triangular _}.find{divisors2(_).length > count}.get

println(a1(5))
println(a2(5))
println(a1(50))
println(a2(50))
println(a1(100))
println(a2(100))
println(a1(500))
println(a2(500))
