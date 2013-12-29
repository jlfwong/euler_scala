def toWords(n: Int): String = {
  val ZeroToNineteen = List(
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen",
    "sixteen",
    "seventeen",
    "eighteen",
    "nineteen"
  )
  val Decades = List(
    "",
    "ten",
    "twenty",
    "thirty",
    "forty",
    "fifty",
    "sixty",
    "seventy",
    "eighty",
    "ninety"
  )
  if (n < 20) ZeroToNineteen(n)
  else if (n < 100) {
    if (n % 10 > 0) Decades(n / 10) + "-" + toWords(n % 10)
    else Decades(n / 10)
  }
  else if (n < 1000) {
    if (n % 100 > 0) ZeroToNineteen(n / 100) + " hundred and " + toWords(n % 100)
    else ZeroToNineteen(n / 100) + " hundred"
  }
  else {
    assert(n == 1000)
    "one thousand"
  }
}

def a1(max: Int) = (1 to max).map{toWords}.mkString.filter{_.isLetter}.length

println(a1(5))
println(a1(1000))


