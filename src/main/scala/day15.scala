import BoardUtils.*
import scala.collection.mutable.PriorityQueue

object day15 extends App:

  def scale(xScale: Int, yScale: Int): Board[Int] => Board[Int] = brd =>
    val brdWith = brd.keySet.maxBy(_.x).x + 1
    val brdHeight = brd.keySet.maxBy(_.y).y + 1

    val scalers = for {
      xD <- (0 until xScale)
      yD <- (0 until yScale)
    } yield (xD, yD)

    scalers
      .flatMap((xD, yD) =>
        brd.map((p, v) => {
          val nwPoint = p + Vec(xD * brdWith, yD * brdHeight)
          val nwValue = (v + xD + yD) % 9
          nwPoint -> (if (nwValue == 0) 9 else nwValue)
        })
      )
      .toMap

  /** A graph is a function which given a Node returns all Nodes which can be reached + the cost of reaching the that
    * Node
    */
  type Graph[N] = N => Map[N, Int]

  /** Dijkstra returns a mapping from node to predecessor seen from the root node, we could also add the cost of getting
    * to the Node :(Map[N, Int], Map[N, N])
    */
  def dijkstra[N](g: Graph[N])(source: N): (Map[N, Int], Map[N, N]) =
    def helper(frontier: PriorityQueue[(N, Int)], costs: Map[N, Int], connected: Map[N, N]): (Map[N, Int], Map[N, N]) =
      // if the frontier is empty, there is nothing left to find
      if (frontier.isEmpty) costs -> connected
      else
        // get the cheapest route first
        val (current, currentCost) = frontier.dequeue

        val neighbors = g(current)
          .filter((n, v) => (currentCost + v) < costs.getOrElse(n, Int.MaxValue))
          .map((n, v) => n -> (currentCost + v))

        val nwFrontier = frontier.addAll(neighbors)
        val preds = neighbors.map((k, v) => k -> current)

        helper(nwFrontier, costs ++ neighbors, connected ++ preds)

    given Ordering[(N, Int)] = Ordering.by(-_._2)
    val frontier = PriorityQueue(source -> 0)
    helper(frontier = frontier, costs = Map(source -> 0), connected = Map.empty[N, N])

  def shortestPath[N](g: Graph[N])(source: N, target: N): Option[List[N]] = {
    val pred = dijkstra(g)(source)._2
    if (pred.contains(target) || source == target)
      Some(iterateRight(target)(pred.get))
    else None
  }

  def iterateRight[N](x: N)(f: N => Option[N]): List[N] = {
    def go(x: N, acc: List[N]): List[N] = f(x) match {
      case None    => x :: acc
      case Some(y) => go(y, x :: acc)
    }

    go(x, List.empty)
  }

  val boardSrc: Board[Int] =
    "day15".live.zipWithIndex.flatMap((line, y) => line.zipWithIndex.map((c, x) => Vec(x, y) -> c.toString.toInt)).toMap

  val board = scale(5, 5)(boardSrc)
  val lr = board.keySet.maxBy(k => k.x + k.y)
  val graph: Graph[Vec] = v => straightMoves.map(m => v + m).filter(board.keySet).map(m => m -> board(m)).toMap

  dijkstra(graph)(Vec(0, 0))._1(lr) andThenShow
