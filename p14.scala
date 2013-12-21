import scala.collection.mutable.Map

def collatz(n: Long) =
  if (n % 2 == 0) n/2
  else 3*n + 1

var memo = Map(1L -> 1L)

def a1(max: Long): (Long, Long) = {
  def collatzLength(n: Long): Long = {
    var path = List(n)
    while (!memo.contains(path.head)) {
      path = collatz(path.head) :: path
    }
    var ret = memo(path.head)
    while (!path.isEmpty) {
      memo(path.head) = ret
      path = path.tail
      ret += 1
    }
    ret - 1
  }

  (1L until max) map{n: Long => (n, collatzLength(n))} maxBy{case (n, len) => len}
}

def a2(max: Long): (Long, Long) = {
  def collatzLength(n: Long, ret: Long = 1): Long =
    if (n == 1) ret
    else collatzLength(collatz(n), ret + 1)

  (1L until max) map{n: Long => (n, collatzLength(n))} maxBy{case (n, len) => len}
}

println(a1(1000000))
println(a2(1000000))
