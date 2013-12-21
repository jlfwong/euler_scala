import scala.collection.mutable.Map

def collatz(n: Long) =
  if (n % 2 == 0) n/2
  else 3*n + 1

var memo = Map(1L -> 1L)

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
  ret
}

def a1(max: Long): (Long, Long) =
  (2L until max) map{n: Long => (n, collatzLength(n))} maxBy{case (n, len) => len}

println(a1(10))
println(a1(100))
println(a1(1000))
println(a1(10000))
println(a1(100000))
println(a1(1000000))
