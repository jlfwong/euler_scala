def a1(max: Int): Long = {
  def square(x: Int) = x * x
  square((1 to max).sum) - (1 to max).map(square _).sum
}

println(a1(10))
println
println(a1(100))
