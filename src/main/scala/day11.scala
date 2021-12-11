
object day11 extends App:
  import BoardUtils.* 

  lazy val field: Board[Int] =
    "day11".live.zipWithIndex.flatMap((line, y) => line.zipWithIndex.map((c, x) => Vec(x, y) -> c.asDigit)).toMap

  def transform(field: Board[Int]): Board[Int] =
    def helper(pointMap: Board[Int], activated: Set[Vec]): (Board[Int]) =
      pointMap.filter((k, v) => (v > 9) & !activated(k)) match
        case inc if inc.isEmpty => pointMap
        case inc => {
          val increasePerPoint = inc.keys.toList.flatMap(i => moves.map(_ + i)).frequency
          val nwPointMap = pointMap.map((k, v) => k -> (v + increasePerPoint.getOrElse(k, 0)))
          helper(nwPointMap, activated ++ inc.keySet)
        }
    helper(field.map((k, v) => k -> (v + 1)), Set.empty[Vec]).map((k, v) => k -> (if (v > 9) 0 else v))

  def transformFor(f: Board[Int], steps: Int) = 
     (1 to steps).foldLeft((f, 0)) {
         (acc, _) => acc match 
             case (field, flashes) =>  
                val nw = transform(field)
                nw -> (nw.count((_, v) => v == 0) + flashes)
     }

  lazy val iterationsUnit: (Board[Int], Int) => Int = (board, steps) => 
      if (board.count((_, v) => v > 0) == 0) steps
      else iterationsUnit(transform(board), steps + 1)

  transformFor(field, 100)._2 andThenShowWith "ex1"

  iterationsUnit(field, 0) andThenShowWith "ex2"
