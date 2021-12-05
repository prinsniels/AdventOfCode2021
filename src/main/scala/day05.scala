
object day05 extends App:
  case class Point(x: Int, y: Int):
    def +(other: Point) = Point(x + other.x, y + other.y) 

  val parse: String => (Point, Point) = { case s"${x1},${y1} -> ${x2},${y2}" =>
    Point(x1.toInt, y1.toInt) -> Point(x2.toInt, y2.toInt)
  }

  val deltaValue: (Int, Int) => Int = (l,r) => if (l < r) 1 else if (l == r) 0 else -1
  
  def toPoints(p1: Point, p2: Point): List[Point] =
    def helper(cur: Point, delta: Point, acc: List[Point]): List[Point] =
      if (cur == p2) cur :: acc
      else helper( cur + delta, delta, cur :: acc)
    helper(p1, Point(deltaValue(p1.x, p2.x), deltaValue(p1.y, p2.y)), List.empty[Point])

  def merge[A](m1: Map[A, Int], m2: Map[A, Int]): Map[A, Int] =
    (m1 ++ m2.map((k, v) => k -> (m1.getOrElse(k, 0) + v)))

  println(
    "day5".live
      .map(parse)
      .filter((l, r) => l.x == r.x || l.y == r.y)
      .map((x, y) => toPoints(x, y).map(_ -> 1).toMap)
      .reduce(merge)
      .filter((p, i) => i >= 2)
      .size
  )
  println(
    "day5".tst
      .map(parse)
      .map((x, y) => toPoints(x, y).map(_ -> 1).toMap)
      .reduce(merge)
      .filter((p, i) => i >= 2)
      .size
  )
