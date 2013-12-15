def primeFactors(num: Int): List[Int] = {
  def pfa(num: Int, div: Int, divs: List[Int]): List[Int] =
    if (num == 1) divs
    else if (num % div == 0) pfa(num / div, 2, div :: divs)
    else pfa(num, div + 1, divs)

  if (num == 1) List(1)
  else pfa(num, 2, Nil)
}

def histogram(nums: Seq[Int]): Map[Int, Int] = {
  nums.groupBy(x => x).mapValues(_.length)
}

def a1(rangeMax: Int): Long = {
  val factors = (1 to rangeMax).map(primeFactors _)
  val factorHists = factors.map(histogram _)
  val lcmFactors = factorHists.reduce {
    (res, cur) => res ++ cur.map {
      case (k, v) => k -> (v max res.getOrElse(k, 0))
    }
  }
  lcmFactors.map{ case (k, v) => Math.pow(k, v).round }.product
}

println(a1(10))
println
println(a1(20))
