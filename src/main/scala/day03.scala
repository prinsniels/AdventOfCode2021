object day3 extends App:
  def addBitString(acc: Array[Int], input: String): Array[Int] =
    acc.zip(input.toCharArray.map(_.toString.toInt)).map(_ + _)

  lazy val mostCommonBitPositions: List[String] => String = data =>
    val summedPositions =
      data.foldLeft(Array.fill(data.head.size)(0))((acc, nxtLine) =>
        addBitString(acc, nxtLine)
      )
    summedPositions.map(x => if (x < data.length / 2.0) 0 else 1).mkString

  lazy val leastCommonBitPositions: List[String] => String = data =>
    mostCommonBitPositions(data).map(x => if (x == '1') '0' else '1')

  def binToInt(inp: String): Int =
    Integer.parseInt(inp, 2)

  val mostCommon = mostCommonBitPositions("day3".live.toList)
  val leastCommon = leastCommonBitPositions("day3".live.toList)

  Integer.parseInt(mostCommon, 2) * Integer.parseInt(leastCommon, 2) andThenShowWith "ex1"

  def O(events: List[String], f: List[String] => String): Int =
    def search(remainder: List[String], pos: Int): Int =
      if (remainder.length == 1) Integer.parseInt(remainder.head, 2)
      else {
        search(remainder.filter(_.charAt(pos) == f(remainder).charAt(pos)), pos + 1)}
    search(events, 0)

  O("day3".live.toList, mostCommonBitPositions) * O("day3".live.toList, leastCommonBitPositions) andThenShowWith "ex2"