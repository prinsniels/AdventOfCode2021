import BoardUtils.*

object day20 extends App:
    
    val bin: (Board[String], String) => Vec => String = 
        (board, d) => v => { 
            for {
                y <- (v.y - 1 to v.y + 1)
                x <- (v.x - 1 to v.x + 1)
                i = if (board.getOrElse(Vec(x, y), d) == ".") "0" else "1"
            } yield i
            }.mkString

    extension (x: String)
      def binInt = Integer.parseInt(x, 2)
    
    def pad(img: Board[String], defualt: String): Board[String] = 
        // image padding is required, the corners coud contain
        // a light pixel, possibly resulting in a new light pixel
        val minX: Int = img.keys.minBy(k => k.x).x
        val minY: Int = img.keys.minBy(k => k.y).y
        val maxX: Int = img.keys.maxBy(k => k.x).x
        val maxY: Int = img.keys.maxBy(k => k.y).y
        
        // add row Above and Below
        val aboveAndBelow = (minX to maxX).flatMap( x => List(Vec(x, minY -1), Vec(x, maxY + 1))).map(_ -> defualt)
        val leftAndRight = (minY -1 to maxY + 1).flatMap(y => List(Vec(minX - 1 , y), Vec(maxX + 1, y))).map(_ -> defualt)

        img ++ aboveAndBelow ++ leftAndRight 
    
    
    def enhance(alg: Map[Int, String], iimg: Board[String], default: String): Board[String] =  
        iimg.keys.map(k => k -> bin(iimg, default)(k)).map( (k, v) => k -> alg.getOrElse(v.binInt, default)).toMap

    def solve(alg: Map[Int, String], iimg: Board[String], steps: Int): Board[String] =
        if (steps <= 0 ) iimg
        else 
            val d = if (steps % 2 == 0) alg(511) else alg(0)
            solve(alg, enhance(alg, pad(iimg, d), d), steps - 1)
    

    val alg = "day20".live.take(1).toList.head.zipWithIndex.map((c, i) => i -> c.toString).toMap
    val iimg =  "day20".live.drop(2).zipWithIndex.flatMap((line, y) => line.zipWithIndex.map((c, x) => Vec(x, y) -> c.toString)).toMap

    assert (alg.keys.size == 512)

    lazy val ex1 = solve(alg, iimg, 2)
    ex1.count( (k, v) => v == "#") andThenShowWith "ex1"

    lazy val ex2 = solve(alg, iimg, 50)
    ex2.count( (k, v) => v == "#") andThenShowWith "ex2"
