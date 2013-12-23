lazy val pascalsTriangle: Stream[List[BigInt]] = List(BigInt(1)) #:: Stream.from(1).map{ row: Int =>
  val prevRow = pascalsTriangle(row-1)
  List(BigInt(1)) ++ (1 until row).map{col: Int => prevRow(col-1) + prevRow(col)} ++ List(BigInt(1))
}

def a1(maxN: Int, threshold: BigInt) =
  pascalsTriangle.take(maxN+1).flatten.filter{_ > threshold}.length

println(pascalsTriangle.take(20).map{_.mkString(" ")}.mkString("\n"))
println(a1(100, BigInt(1000000)))