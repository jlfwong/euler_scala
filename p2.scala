def fib(): Iterator[Int] = {
  var a = 1
  var b = 1
  var i = 0
  def next(): Int = {
    i += 1
    i match {
      case 1 => 1
      case 2 => 1
      case _ => {
        val temp = a
        a = b
        b = b + temp
        b
      }
    }
  }
  Iterator.continually(next())
}

def a1(max: Int): Int =
  fib().takeWhile(x => x < max).filter(x => x % 2 == 0).sum

println(a1(4000000))
