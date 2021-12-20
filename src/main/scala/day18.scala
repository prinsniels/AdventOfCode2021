import scala.annotation.tailrec
import day18.X

object day18 extends App:

  trait X
  case class D(v: Int) extends X:
    override def toString = v.toString
  case class C(v: String) extends X:
    override def toString = v.toString

  object X:
    def apply(s: String) =
      s.split(",").toVector.flatMap(x => x.toVector.map(parse))

    def empty: Vector[X] =
      X("")

    def parse(c: Char): X =
      if (c.isDigit) D(c.toString.toInt)
      else C(c.toString)

  @tailrec
  def addFromEnd(c: Int, v: Vector[X], r: Vector[X]): Vector[X] =
    v match
      case xs :+ D(x) => xs ++ Vector(D(c + x)) ++ r
      case xs :+ x    => addFromEnd(c, xs, x +: r)
      case _          => r

  @tailrec
  def addFromStart(c: Int, v: Vector[X], r: Vector[X]): Vector[X] =
    v match
      case D(x) +: xs => (r :+ D(x + c)) ++ xs
      case x +: xs    => addFromStart(c, xs, r :+ x)
      case _          => r

  @tailrec
  def removeLast(x: X, v: Vector[X], r: Vector[X]): Vector[X] =
    if (v.isEmpty) r
    else if (v.last == x) v.init :++ r
    else removeLast(x, v.init, r.prepended(v.last))

  @tailrec
  def removeFirst(x: X, v: Vector[X], r: Vector[X]): Vector[X] =
    if (v.isEmpty) r
    else if (v.head == x) r :++ v.tail
    else removeFirst(x, v.tail, r.appended(v.head))

  def explode(v: Vector[X]): Either[Vector[X], Vector[X]] =
    def scan(remain: Vector[X], prev: Vector[X], depth: Int): Either[Vector[X], Vector[X]] =
      if (remain.isEmpty) Right(prev)
      else {
        remain.head match
          case C(c) if c == "[" => scan(remain.tail, prev.appended(remain.head), depth + 1)
          case C(c) if c == "]" => scan(remain.tail, prev.appended(remain.head), depth - 1)
          case o @ D(d) if depth >= 5 =>
            Left(
              addFromEnd(d, removeLast(C("["), prev, X.empty), X.empty)
                ++
                  X("0")
                  ++
                  addFromStart(
                    remain.tail.head.asInstanceOf[D].v,
                    removeFirst(C("]"), remain.tail.tail, X.empty),
                    X.empty
                  )
            )
          case _ => scan(remain.tail, prev.appended(remain.head), depth)
      }
    scan(v, Vector.empty, 0)

  def splitFirst(v: Vector[X]): Either[Vector[X], Vector[X]] =
    def scan(remain: Vector[X], preceded: Vector[X]): Either[Vector[X], Vector[X]] =
      remain match
        case D(x) +: xs if (x >= 10) =>
          Left(preceded :++ Vector(C("["), D(math.floor(x / 2.0).toInt), D(math.ceil(x / 2.0).toInt), C("]")) :++ xs)
        case x +: xs => scan(xs, preceded.appended(x))
        case _       => Right(preceded)

    scan(v, Vector.empty)

  def solve(s: Vector[X]): Vector[X] =
    explode(s).flatMap(r => splitFirst(r)) match
      case Left(v)  => solve(v)
      case Right(v) => v

  def so(sample: List[String]) =
    lazy val data = sample.map(f => X(f))
    data.tail.foldLeft(data.head)((l, r) => solve(X("[") ++ l ++ r ++ X("]")))

  def combine(xs: Vector[X]): Vector[X] =
    def helper(remain: Vector[X], pre: Vector[X]): Vector[X] =
      remain match
        case D(x1) +: D(x2) +: xs => {
          removeLast(C("["), pre, X.empty) ++ Vector(D(x1 * 3 + x2 * 2)) ++ removeFirst(C("]"), xs, X.empty)
        }
        case x +: xs => helper(xs, pre :+ x)
    helper(xs, X.empty)

  def sum(sample: Vector[X]): Int =
    if (sample.length == 1) sample.head.asInstanceOf[D].v
    else sum(combine(sample))

  lazy val data = "day18".live.toList

  sum(so(data)) andThenShowWith "ex1"

  data.combinations(2).flatMap(x => List(x, x.reverse)).map(x => sum(so(x))).max andThenShowWith "ex2"
