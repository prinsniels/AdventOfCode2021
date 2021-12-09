import scala.annotation.tailrec
object day09 extends App:
  case class Vec(x: Int, y: Int):
    def +(other: Vec): Vec =
      Vec(x + other.x, y + other.y)

  val moves = for {
    x <- (-1 to 1)
    y <- (-1 to 1)
    if (x == 0 || y == 0) && x != y
  } yield Vec(x, y)

  def flood(field: Map[Vec, Int], startPoint: Vec, moves: Seq[Vec]): Set[Vec] =
    @tailrec
    def helper(options: List[Vec], taken: Set[Vec], tried: Set[Vec]): Set[Vec] =
      options match
        case Nil => taken
        case x :: xs => {
          if (tried(x)) helper(xs, taken, tried)
          else if (field.getOrElse(x, 9) == 9) helper(xs, taken, tried + x)
          else
            helper(
              moves.map(_ + x).filterNot(tried).toList ++ xs,
              taken + x,
              tried + x
            )
        }

    helper(List(startPoint), Set.empty[Vec], Set.empty[Vec])

  val field: Map[Vec, Int] = "day9".tst.zipWithIndex
    .flatMap((line, y) =>
      line.zipWithIndex.map((height, x) => Vec(x, y) -> height.toString.toInt)
    )
    .toMap

  val lowPoints =
    field.filter((k, v) => !moves.exists(m => field.getOrElse(k + m, 99) <= v))
  
  lowPoints.map((k, v) => v + 1).sum andThenShowWith "ex1"

  lowPoints
    .map((point, value) => flood(field, point, moves))
    .toSet
    .toList
    .map(v => v.size)
    .sortBy(-_)
    .take(3)
    .reduce(_ * _) andThenShowWith "ex2"
