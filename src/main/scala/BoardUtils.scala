object BoardUtils:
  case class Vec(x: Int, y: Int):
    def +(other: Vec): Vec =
      Vec(x + other.x, y + other.y)

  lazy val diagonalMoves: List[Vec] = (for {
      x <-  List(-1 , 1)
      y <-  List(-1 , 1)
    } yield Vec(x, y)).toList 

  lazy val straightMoves: List[Vec] = List(-1 , 1).flatMap(x => List(Vec(0, x), Vec(x, 0)))

  lazy val moves: List[Vec] = diagonalMoves ++ straightMoves

  type Board[A] = Map[Vec, A]

  extension [A](board: Board[A])
    def showMapHighlightByKey(pred: Vec => Boolean): Board[A] =
      import io.AnsiColor._

      (board.keys.map(_.y).min to board.keys.map(_.y).max)
        .map(y =>
          (board.keys.map(_.x).min to board.keys.map(_.x).max)
            .map(x => {
              if (pred(Vec(x, y))) RED_B + board.getOrElse(Vec(x, y), ' ').toString + RESET
              else board.getOrElse(Vec(x, y), ' ').toString
            })
            .mkString
        )
        .foreach(println)
      board

    def showMap: Board[A] =
      showMapHighlightByKey(_ => false)

    def showMapHighlightByValue(pred: A => Boolean): Board[A] =
      showMapHighlightByKey(board.filter((_, v) => pred(v)).keySet)
