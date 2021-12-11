import scala.annotation.tailrec
object day09 extends App:
  import BoardUtils.* 

  def flood(field: Board[Int], startPoint: Vec, moves: Seq[Vec]): Set[Vec] =
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

  val field: Board[Int] = "day9".tst.zipWithIndex
    .flatMap((line, y) =>
      line.zipWithIndex.map((height, x) => Vec(x, y) -> height.toString.toInt)
    )
    .toMap

  val lowPoints =
    field.filter((k, v) => !straightMoves.exists(m => field.getOrElse(k + m, 99) <= v))
  
  lowPoints.map((k, v) => v + 1).sum andThenShowWith "ex1"

  lowPoints
    .map((point, value) => flood(field, point, straightMoves))
    .toSet
    .toList
    .map(v => v.size)
    .sortBy(-_)
    .take(3)
    .reduce(_ * _) andThenShowWith "ex2"
