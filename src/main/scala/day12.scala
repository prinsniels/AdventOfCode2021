object day12 extends App:

  trait Cave
  case class Small(v: String) extends Cave
  case class Big(v: String) extends Cave
  case object Start extends Cave
  case object End extends Cave

  object Cave:
    def apply(s: String): Cave =
      s match
        case "start"                  => Start
        case "end"                    => End
        case c if c.exists(_.isUpper) => Big(c)
        case c                        => Small(c)

  lazy val caveSystem = "day12".tst
    .flatMap(line => {
      val caves = line.split("-").toVector.map(s => Cave(s))
      List(caves(0) -> caves(1), caves(1) -> caves(0))
    })
    .foldLeft(Map.empty[Cave, Set[Cave]]) { case (map, line) =>
      map + (line._1 -> (map.getOrElse(line._1, Set.empty) + line._2))
    }

  val allowedEx1: Set[Cave] => Cave => Boolean =
    visited =>
      cave =>
        cave match
          case Start        => false
          case c @ Small(_) => !visited(cave)
          case _            => true

  def getAllPaths(caveSystem: Map[Cave, Set[Cave]]) =
    def helper(ongoing: List[(List[Cave], Set[Cave])], completed: List[List[Cave]]): List[List[Cave]] =
      ongoing match
        case Nil => completed
        case x :: xs => {
          val (path, visited) = x
          if (path.head == End) helper(xs, path.reverse :: completed)
          else
            val nwOngoing = caveSystem(path.head)
              .filter(allowedEx1(visited))
              .toList
              .map(nc => (nc :: path) -> (visited + path.head)) ++ xs
            helper(nwOngoing, completed)
        }

    helper(List(List(Start) -> Set.empty), List.empty)

//   getAllPaths(caveSystem).size andThenShowWith "ex1"

  def getAllPaths2(caveSystem: Map[Cave, Set[Cave]]) =
    def helper(ongoing: List[(List[Cave], Set[Cave], Boolean)], completed: List[List[Cave]]): List[List[Cave]] =
      ongoing match
        case Nil => completed
        case x :: xs => {
          val (path, visited, extraAvailable) = x
          if (path.head == End) helper(xs, path.reverse :: completed)
          else
            val nwOngoing = caveSystem(path.head)
              .filter(allowedEx2(visited)(extraAvailable))
              .toList
              .map(nc => ((nc :: path), (visited + path.head), still(extraAvailable)(visited)(nc))) ++ xs
            helper(nwOngoing, completed)
        }

    helper(List((List(Start), Set.empty, true)), List.empty)

  val allowedEx2: Set[Cave] => Boolean => Cave => Boolean =
    visited =>
      extraAvailable =>
        cave =>
          cave match
            case Start        => false
            case c @ Small(_) => extraAvailable | !visited(cave)
            case _            => true

  val still: Boolean => Set[Cave] => Cave => Boolean =
    extraAvailable =>
      visited =>
        cave =>
          cave match
            case s @ Small(_) =>
              if (extraAvailable & !visited(s)) true
              else if (extraAvailable & visited(s)) false
              else false
            case _ => extraAvailable

  getAllPaths2(caveSystem).size andThenShowWith "ex2"
