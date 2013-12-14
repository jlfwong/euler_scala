def largestPrimeFactor(num: Long): Long = {
  def lpfcur(num: Long, cur: Long): Long =
    if (num == cur) num
    else if (num % cur == 0) lpfcur(num / cur, 2)
    else lpfcur(num, cur + 1)

  lpfcur(num, 2)
}

println(largestPrimeFactor(13195))
println(largestPrimeFactor(600851475143L))
