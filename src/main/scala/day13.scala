import javax.swing.plaf.metal.MetalIconFactory.FolderIcon16

object day13 extends App:
  case class Point(x: Int, y: Int)

  trait Fold
  case class FoldY(v: Int) extends Fold
  case class FoldX(x: Int) extends Fold

  def parsePoint(line: String): Option[Point] =
    line match
      case s"${x},${y}" => Some(Point(x.toInt, y.toInt))
      case _            => None

  def parseFold(line: String): Option[Fold] =
    line match
      case s"fold along x=${x}" => Some(FoldX(x.toInt))
      case s"fold along y=${y}" => Some(FoldY(y.toInt))
      case _                    => None

  val mod: (Int, Int) => Int =
    (c, f) => if (c < f) c else f - (c - f)

  def foldSheet(points: Set[Point], fold: Fold): Set[Point] =
    fold match
      case FoldX(x) => points.map(p => p.copy(x = mod(p.x, x)))
      case FoldY(y) => points.map(p => p.copy(y = mod(p.y, y)))

  val data = "day13".live.toList

  val points = data.map(parsePoint).collect { case Some(p) => p }.toSet
  val folds = data.map(parseFold).collect { case Some(f) => f }.toList

  foldSheet(points, folds.head).size andThenShowWith "ex1"

  val res = folds.foldLeft(points)(foldSheet)

  (0 to res.map(_.y).max)
    .map(y => (0 to res.map(_.x).max).map(x => if (res(Point(x, y))) "*" else " ").mkString)
    .foreach(println)
