
object day05 extends App:
  case class Point(x: Int, y: Int):
    def +(other: Point) = Point(x + other.x, y + other.y) 

  val parse: String => (Point, Point) = { case s"${x1},${y1} -> ${x2},${y2}" =>
    Point(x1.toInt, y1.toInt) -> Point(x2.toInt, y2.toInt)
  }

  def toPointMap(p1: Point, p2: Point): Map[Point, Int] =
    val xd = if (p1.x < p2.x) 1 else if (p1.x == p2.x) 0 else -1
    val yd = if (p1.y < p2.y) 1 else if (p1.y == p2.y) 0 else -1
    
    def helper(cur: Point, delta: Point, acc: List[Point]): List[Point] =
      if (cur == p2) cur :: acc
      else helper( cur + delta, delta, cur :: acc)

    helper(p1, Point(xd, yd), List.empty[Point]).map(_ -> 1).toMap

  def merge[A](m1: Map[A, Int], m2: Map[A, Int]): Map[A, Int] =
    (m1 ++ m2.map((k, v) => k -> (m1.getOrElse(k, 0) + v)))

  println(
    "day5".live
      .map(parse)
      .filter((l, r) => l.x == r.x || l.y == r.y)
      .map(toPointMap)
      .reduce(merge)
      .filter((p, i) => i >= 2)
      .size
  )
  println(
    "day5".live
      .map(parse)
      .map(toPointMap)
      .reduce(merge)
      .filter((p, i) => i >= 2)
      .size
  )
