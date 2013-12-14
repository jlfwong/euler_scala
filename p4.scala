def a1(digits: Int): Long = {
  val low =  Math.round(Math.pow(10, digits - 1))
  val high = Math.round(Math.pow(10, digits) - 1)

  (for {
    x <- (low to high)
    y <- (x to high)
    prod = x * y
    if prod.toString == prod.toString.reverse
  } yield prod).max
}

println(a1(2))
println
println(a1(3))
