import Math.{min, max, abs}

object day22 extends App:

  trait Switch
  case class On(c: Cube) extends Switch
  case class Off(c: Cube) extends Switch

  def parse(line: String): Switch =
    line match
      case s"on x=${x1}..${x2},y=${y1}..${y2},z=${z1}..${z2}" =>
        On(Cube(x1.toInt, x2.toInt, y1.toInt, y2.toInt, z1.toInt, z2.toInt))

      case s"off x=${x1}..${x2},y=${y1}..${y2},z=${z1}..${z2}" =>
        Off(Cube(x1.toInt, x2.toInt, y1.toInt, y2.toInt, z1.toInt, z2.toInt))

  case class Cube(xl: Int, xh: Int, yl: Int, yh: Int, zl: Int, zh: Int):
    def intersects(o: Cube): Boolean =
      xl <= o.xh && xh >= o.xl &&
        yl <= o.yh && yh >= o.yl &&
        zl <= o.zh && zh >= o.zl

    def overlaps(o: Cube): Boolean =
      xl <= o.xl && xh >= o.xh &&
        yl <= o.yl && yh >= o.yh &&
        zl <= o.zl && zh >= o.zh

    val volume: Long =
      /*
         -1, 0, 1
         -1, 0, 1
         -1, 0, 1

         should become 3 * 3 * 3
       */
      (abs(xh - xl).toLong + 1L) * (abs(yh - yl) + 1L).toLong * (abs(zh - zl).toLong + 1L)

  def slice(ex: Cube, nw: Cube): List[Cube] =
    // Note: R is merged into the L

    // if the incoming (r) completely overlaps the incoming
    // remove the old one completely, it will be reduced to nothing
    if (nw.overlaps(ex)) List.empty
    else if (nw.intersects(ex)) {
      val nwXl = max(ex.xl, nw.xl)
      val nwXh = min(ex.xh, nw.xh)

      val nwYl = max(ex.yl, nw.yl)
      val nwYh = min(ex.yh, nw.yh)

      val nwZl = max(ex.zl, nw.zl)
      val nwZh = min(ex.zh, nw.zh)

      val top = Cube(ex.xl, ex.xh, nw.yh + 1, ex.yh, ex.zl, ex.zh)
      val bottom = Cube(ex.xl, ex.xh, ex.yl, nw.yl - 1, ex.zl, ex.zh)

      val left = Cube(ex.xl, nw.xl - 1, nwYl, nwYh, ex.zl, ex.zh)
      val right = Cube(nw.xh + 1, ex.xh, nwYl, nwYh, ex.zl, ex.zh)

      val back = Cube(nwXl, nwXh, nwYl, nwYh, nw.zh + 1, ex.zh)
      val front = Cube(nwXl, nwXh, nwYl, nwYh, ex.zl, nw.zl - 1)

      // when the lower exceeds the upper the incoming was partly outside on that surface
      List(top, bottom, left, right, back, front).filter(x => x.xh >= x.xl && x.yh >= x.yl && x.zh >= x.zl)
    } else List(ex)

  def inContext(c: Cube): Boolean =
    c.intersects(Cube(-50, 50, -50, 50, -50, 50))

  def clip(c: Cube): Cube =
    c.copy(
      xl = max(-50, c.xl),
      xh = min(50, c.xh),
      yl = max(-50, c.yl),
      yh = min(50, c.yh),
      zl = max(-50, c.zl),
      zh = min(50, c.zh)
    )

  "day22".live
    .map(parse)
    .filter {
      case On(c)  => inContext(c)
      case Off(c) => inContext(c)
    }
    .map {
      case On(c)  => On(clip(c))
      case Off(c) => Off(clip(c))
    }
    .foldLeft(List.empty[Cube]) {
      case (reactor, On(c))  => c :: reactor.flatMap(ex => slice(ex, c))
      case (reactor, Off(c)) => reactor.flatMap(ex => slice(ex, c))
    }
    .map(_.volume.toLong)
    .sum andThenShow

  "day22".live
    .map(parse)
    .foldLeft(List.empty[Cube]) {
      case (reactor, On(c))  => c :: reactor.flatMap(ex => slice(ex, c))
      case (reactor, Off(c)) => reactor.flatMap(ex => slice(ex, c))
    }
    .map(_.volume)
    .sum andThenShow
