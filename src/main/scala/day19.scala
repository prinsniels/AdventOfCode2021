import day19.Scanner

object day19 extends App:
  case class Scanner(id: Int, beacons: Set[V], origen: V)
  case class V(x: Int, y: Int, z: Int):
    def -(other: V): V =
      V(x - other.x, y - other.y, z - other.z)

    def +(other: V): V =
      V(x + other.x, y + other.y, z + other.z)

  def rotations: List[V => V] =
    // all possible rotations to apply, should end up to be 24
    // a rotation should be a function from A => A
    List(
      in => V(in.x, in.y, in.z),
      in => V(-in.y, in.x, in.z),
      in => V(-in.x, -in.y, in.z),
      in => V(in.y, -in.x, in.z),
      in => V(-in.x, in.y, -in.z),
      in => V(in.y, in.x, -in.z),
      in => V(in.x, -in.y, -in.z),
      in => V(-in.y, -in.x, -in.z),
      in => V(-in.z, in.y, in.x),
      in => V(-in.z, in.x, -in.y),
      in => V(-in.z, -in.y, -in.x),
      in => V(-in.z, -in.x, in.y),
      in => V(in.z, in.y, -in.x),
      in => V(in.z, in.x, in.y),
      in => V(in.z, -in.y, in.x),
      in => V(in.z, -in.x, -in.y),
      in => V(in.x, -in.z, in.y),
      in => V(-in.y, -in.z, in.x),
      in => V(-in.x, -in.z, -in.y),
      in => V(in.y, -in.z, -in.x),
      in => V(in.x, in.z, -in.y),
      in => V(-in.y, in.z, -in.x),
      in => V(-in.x, in.z, in.y),
      in => V(in.y, in.z, in.x)
    )

  lazy val scannerRotations: Scanner => List[Scanner] =
    s => rotations.map(fv => s.copy(beacons = s.beacons.map(fv)))

  lazy val parse: List[String] => List[Scanner] = _.filterNot(_ == "")
    .foldLeft(List.empty[Scanner])((acc, line) =>
      line match
        case s"--- scanner ${id} ---" => Scanner(id.toInt, Set.empty[V], V(0, 0, 0)) :: acc
        case s"${x},${y},${z}" => acc.head.copy(beacons = acc.head.beacons + V(x.toInt, y.toInt, z.toInt)) :: acc.tail
    )

  def connect(l: Scanner, r: Scanner): Option[Scanner] =
    /** try for each beacon if aligning it with a beacon from r results in a modded r.beacons if overlap exists, if true
      * the connection is completed
      */
    l.beacons
      .flatMap(lb => r.beacons.map(rb => Scanner(r.id, r.beacons.map(_ - (rb - lb)), r.origen - (rb - lb))))
      .find(rr => (l.beacons & rr.beacons).size >= 12)

  def testScannerOverlap(l: Scanner, r: Scanner): Option[Scanner] =
    /** For all rotations, return the first overlapping configuration of the scanner modified, relative to the left one
      */
    scannerRotations(r).map(connect(l, _)).find(x => !x.isEmpty).getOrElse(None)

  lazy val tst = parse("day19".tst.toList).reverse
  lazy val live = parse("day19".live.toList).reverse

  def connectScanners(i: List[Scanner]): List[Scanner] =
    def helper(frontier: List[Scanner], visited: Set[Int], total: List[Scanner]): List[Scanner] =
      frontier match
        case Nil => total
        case l :: xs =>
          val related =
            i.filterNot(s => visited(s.id)).filterNot(_.id == l.id).map(r => testScannerOverlap(l, r)).collect {
              case Some(s) => s
            }
          helper(xs ++ related, visited + l.id, total ++ related)
    helper(List(i.head), Set.empty, List(i.head))

  def manhattan(l: V, r: V) =
    math.abs(l.x - r.x) + math.abs(l.y - r.y) + math.abs(l.z - r.z)

  lazy val full = connectScanners(live)

  full.map(_.beacons).reduce(_ ++ _).size andThenShowWith "ex1"

  full.combinations(2).map(x => x.head -> x.tail.head).map((l, r) => manhattan(l.origen, r.origen)).max andThenShowWith "ex2"
