import scala.annotation.tailrec
object day06 extends App:

  type State = Map[Int, Long]

  val startState =
    "day6".live.toList.head.split(',').map(_.toInt).toList.frequency.map((k, v) => k -> v.toLong)

  @tailrec
  def process(state: State, turns: Int, transformation: State => State): State =
    if (turns == 0) state
    else process(transformation(state), turns - 1, transformation)

  def transform(origin: State): State =
    origin
      .map {
        case (0, v) => List(8 -> v, 6 -> v).toMap
        case (k, v) => List(k - 1 -> v).toMap
      }
      .reduce((l, r) => l ++ r.map((k, v) => k -> (l.getOrElse(k, 0L) + v)))

  println(process(startState, 80, transform).map((_, v) => v).sum)
  println(process(startState, 256, transform).map((_, v) => v).sum)