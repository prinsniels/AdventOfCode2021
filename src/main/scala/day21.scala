
object day21 extends App:

  case class Player(name: String, position: Int, score: Int)
  case class Game(players: List[Player], d: Dice)
  case class Dice(throws: Int, face: Int)

  def changePLayerPositionBy(p: Player, i: Int): Player =
    val nwPos = if ((p.position + i) % 10 == 0) 10 else (p.position + i) % 10
    p.copy(position = nwPos, score = p.score + nwPos)

  def diceThrow(dice: Dice): (Int, Dice) =
    val nwFace = if ((dice.face + 1) % 100 == 0) 100 else (dice.face + 1) % 100
    nwFace -> Dice(dice.throws + 1, nwFace)

  val threeThrows: Dice => (Dice, List[Int]) = d =>
    val (v1, d1) = diceThrow(d)
    val (v2, d2) = diceThrow(d1)
    val (v3, d3) = diceThrow(d2)
    (d3, List(v1, v2, v3))

  def step(g: Game): Game =
    val (d, points) = threeThrows(g.d)
    Game(g.players.tail :+ changePLayerPositionBy(g.players.head, points.sum), d)

  def playUntil(g: Game, pred: Game => Boolean): Game =
    if (pred(g)) g
    else playUntil(step(g), pred)

  val d1 = Dice(throws = 0, face = 0)
  val g1 = Game(
    List(
      Player("1", position = 10, score = 0),
      Player("2", position = 2, score = 0)
    ),
    d1
  )

  lazy val f = playUntil(g1, g => g.players.last.score >= 1000)

  (f.players.map(_.score).min * f.d.throws) andThenShowWith "ex1"

  lazy val transformations: Map[Int, Int] =
    val options = for {
      i <- (1 to 3)
      j <- (1 to 3)
      k <- (1 to 3)
    } yield (i + j + k)

    options
      .groupBy(identity)
      .map((k, v) => k -> v.size)

  def quantumStep(game: Game): Map[Game, Int] =
    transformations.map((point, multi) =>
      Game(game.players.tail :+ changePLayerPositionBy(game.players.head, point), game.d) -> multi
    )

  def playQuantumUntil(ongoing: Map[Game, Long], finished: Map[Game, Long]): Map[Game, Long] =
    if (ongoing.isEmpty) finished
    else {
      val rolled = ongoing
        .map((g, m) => quantumStep(g).map((ng, nm) => ng -> (m * nm)))
        .reduce((l, r) => l ++ r.map((rk, rv) => rk -> (rv + l.getOrElse(rk, 0L))))

      val nwOngoing = rolled.filter((g, m) => g.players.last.score < 21)
      val nwFinished = rolled.filterNot((g, m) => g.players.last.score < 21)

      val nxt = finished ++ nwFinished.map((rk, rv) => rk -> (rv + finished.getOrElse(rk, 0L)))
      playQuantumUntil(nwOngoing, nxt)
    }

  playQuantumUntil(Map(g1 -> 1), Map.empty).toList
    .map((g, m) => {
      if (g.players.head.score >= 21) g.players.head.name -> m
      else g.players.tail.head.name -> m
    })
    .foldLeft(Map.empty[String, Long])((acc, nw) => acc + (nw._1 -> (acc.getOrElse(nw._1, 0L) + nw._2))) andThenShowWith "ex2"
