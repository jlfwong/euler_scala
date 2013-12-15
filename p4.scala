def a1(digits: Int): Long = {
  val low =  Math.round(Math.pow(10, digits - 1))
  val high = Math.round(Math.pow(10, digits))

  (for {
    x <- (low until high)
    y <- (x until high)
    prod = x * y
    if prod.toString == prod.toString.reverse
  } yield prod).max
}

println(a1(2))
println
println(a1(3))
