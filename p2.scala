def fib(): Iterator[Int] = {
  var a = 1
  var b = 1
  Iterator(a, b) ++ Iterator.continually {
    val temp = a
    a = b
    b = b + temp
    b
  }
}

def a1(max: Int): Int =
  fib().takeWhile(x => x < max).filter(x => x % 2 == 0).sum

def fibcb(cb: (Int => Boolean)) {
  var a = 1
  var b = 1
  if (!cb(a)) return
  if (!cb(b)) return
  while (true) {
    val temp = a
    a = b
    b = b + temp
    if (!cb(b)) return
  }
}

def a2(max: Int): Int = {
  var sum = 0
  fibcb(x => {
    if (x < max) {
      if (x % 2 == 0) sum += x
      true
    } else false
  })
  sum
}

println(a1(4000000))
println(a2(4000000))
