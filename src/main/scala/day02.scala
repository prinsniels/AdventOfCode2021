object Day2 extends App:

  case class Vec2(depth: Int, horizon: Int):
    lazy val multiple: Int = horizon * depth

  case class Vec3(depth: Int, horizon: Int, aim: Int):
    lazy val multiple: Int = horizon * depth

  trait Move
  case class Forward(v: Int) extends Move
  case class Up(v: Int) extends Move
  case class Down(v: Int) extends Move

  def parse(i: String): Move = i match
    case s"forward ${v}" => Forward(v.toInt)
    case s"up ${v}"      => Up(v.toInt)
    case s"down ${v}"    => Down(v.toInt)

  def move(s: Vec2, m: Move): Vec2 =
    m match
      case Up(v)      => s.copy(depth = s.depth - v)
      case Down(v)    => s.copy(depth = s.depth + v)
      case Forward(v) => s.copy(horizon = s.horizon + v)

  def move(s: Vec3, m: Move): Vec3 =
    m match
      case Up(v)   => s.copy(aim = s.aim - v)
      case Down(v) => s.copy(aim = s.aim + v)
      case Forward(v) =>
        s.copy(horizon = s.horizon + v, depth = s.depth + s.aim * v)

  "day2".live
    .map(parse)
    .foldLeft(Vec2(0, 0))(move)
    .multiple andThenShowWith "ex1"

  "day2".live
    .map(parse)
    .foldLeft(Vec3(0, 0, 0))(move)
    .multiple andThenShowWith "ex2"
