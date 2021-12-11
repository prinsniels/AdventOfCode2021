
object day11 extends App:
  import BoardUtils.* 

  lazy val field: Map[Vec, Int] =
    "day11".live.zipWithIndex.flatMap((line, y) => line.zipWithIndex.map((c, x) => Vec(x, y) -> c.asDigit)).toMap

  def transform(field: Map[Vec, Int]): Map[Vec, Int] =
    def helper(pointMap: Map[Vec, Int], activated: Set[Vec]): (Map[Vec, Int]) =
      pointMap.filter((k, v) => (v > 9) & !activated(k)) match
        case inc if inc.isEmpty => pointMap
        case inc => {
          val increasePerPoint = inc.keys.toList.flatMap(i => moves.map(_ + i)).frequency
          val nwPointMap = pointMap.map((k, v) => k -> (v + increasePerPoint.getOrElse(k, 0)))
          helper(nwPointMap, activated ++ inc.keySet)
        }
    helper(field.map((k, v) => k -> (v + 1)), Set.empty[Vec]).map((k, v) => k -> (if (v > 9) 0 else v))

  def transformFor(f: Map[Vec, Int], steps: Int) = 
     (1 to steps).foldLeft((f, 0)) {
         (acc, _) => acc match 
             case (field, flashes) =>  
                val nw = transform(field)
                nw -> (nw.count((_, v) => v == 0) + flashes)
     }

  def firstAll(f: Map[Vec, Int]): Int =
    def helper(f: Map[Vec, Int], c: Int, pred: Map[Vec, Int] => Boolean): Int = 
        val nw = transform(f)
        if (pred(nw)) c
        else helper(nw, c + 1,  pred)
    helper(f, 0, (x: Map[Vec, Int]) => x.count((_, v) => v > 0) == 0) + 1

  transformFor(field, 100)._2 andThenShowWith "ex1"

  firstAll(field) andThenShowWith "ex2" 


