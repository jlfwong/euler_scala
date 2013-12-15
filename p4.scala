def a1(digits: Int): Long = {
  val low =  Math.pow(10, digits - 1).round
  val high = Math.pow(10, digits).round

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
