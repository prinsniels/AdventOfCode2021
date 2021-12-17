import BoardUtils.Vec

object day17 extends App:
    case class Target(lx: Int, ly: Int, tx: Int, ty: Int)

    case class Probe(pos: Vec, vel: Vec)

    val probeIn: Target => Probe => Boolean = 
        t => p => (p.pos.x <= t.tx) & (p.pos.x >= t.lx) & (p.pos.y <= t.ty) & (p.pos.y >= t.ly)

    def step(probe: Probe): Probe = 
      val nwPos = probe.pos + probe.vel
      val nwX = if (probe.vel.x < 0) probe.vel.x + 1 
                else if (probe.vel.x > 0) probe.vel.x - 1  
                else 0
      val nwY = probe.vel.y - 1
      Probe(nwPos, Vec(nwX, nwY))

    def runUntil(init: Probe, pred: Probe => Boolean): List[Probe] =  
      def helper(probe: Probe, acc: List[Probe]): List[Probe] = 
          if (pred(probe)) acc 
          else helper(step(probe), probe :: acc)
      helper(init, List.empty).reverse

    val overshot: Target => Probe => Boolean = 
        t => p => p.pos.x > t.tx | p.pos.y < t.ly
    
    val ta = Target(lx=20, tx=30, ly = -10, ty = -5)
    val pa = Target(lx=287, tx=309, ly = -76, ty = -48)
    val overshotTarget = overshot(pa)
    val probeInTarget =  probeIn(pa)

    lazy val options = for {
        x <- (0 to 1000)
        y <- (-1000 to 1000)
    } yield (Vec(x, y) -> runUntil(Probe(Vec(0,0), Vec(x, y)), overshotTarget))

    lazy val hittingOptions = options.filter( (_, ps) => ps.exists(probeInTarget))
    
    hittingOptions.map((_, ps) => ps.map(_.pos.y).max ).max andThenShowWith "ex1"
    hittingOptions.size andThenShowWith "ex2"
