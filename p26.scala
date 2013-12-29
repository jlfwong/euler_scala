import scala.collection.mutable

def reciprocalExpansion(divisor: Int) = {
  var dividend = 10
  Iterator.continually {
    val quotient = dividend / divisor
    val remainder = dividend % divisor
    dividend = 10 * (dividend % divisor)
    (quotient, remainder)
  }
}

def reciprocalCycleLength(divisor: Int) = {
  val pos = mutable.Map[Tuple2[Int, Int], Int]()
  var lastKey = (-1, -1)
  var lastRemainder = -1
  var lastIndex = -1
  reciprocalExpansion(divisor).zipWithIndex.takeWhile{
      case (key @ (_, remainder), index) => {
        lastKey = key
        lastRemainder = remainder
        lastIndex = index
        if (remainder == 0 || pos.contains(key)) false
        else {
          pos(key) = index
          true
        }
      }
  }.toList
  if (lastRemainder == 0) -1
  else lastIndex - pos(lastKey)
}

def a1(max: Int) = (2 to max).map{n: Int => (n, reciprocalCycleLength(n))}.maxBy(_._2)

println(a1(10))
println(a1(1000))
