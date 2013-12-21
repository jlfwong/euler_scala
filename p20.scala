val a1: (Int => BigInt) = 1L #:: Stream.from(1).map {x: Int => x * a1(x - 1)}

def a2(n: Int): BigInt = {
  def factTail(n: Int, ret: BigInt): BigInt =
    if (n == 0) ret
    else factTail(n - 1, n * ret)

  factTail(n, 1)
}


def soln(num: Int, method: (Int => BigInt)) {
  println(method(num).toString.sliding(1).map(_.toInt).sum)
}

soln(10, a1)
soln(10, a2)
soln(100, a1)
soln(100, a2)
