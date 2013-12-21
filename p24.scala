val factorial: (Int => BigInt) = 1L #:: Stream.from(1).map {x: Int => x * factorial(x - 1)}

def a1(num: Int, alpha: String): String = {
  if (alpha.isEmpty) ""
  else {
    val bucketSize = factorial(alpha.length - 1)
    val bucketNum = (num / bucketSize).toInt
    alpha(bucketNum) + a1((num % bucketSize).toInt, alpha.substring(0, bucketNum) + alpha.substring(bucketNum + 1))
  }
}

(1 to 6).map{n: Int => a1(n - 1, "012")}.foreach(println)
println(a1(1000000 - 1, "0123456789"))
