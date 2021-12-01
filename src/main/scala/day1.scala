object Day1 extends App:

  val lines = "day1".live
    .map(_.toInt)
    .toList

  val ex1 = lines.zip(lines.tail).count(_ < _)

  println(ex1)

  val sliding = lines.sliding(3).map(_.sum).toList

  val ex2 = sliding.zip(sliding.tail).count(_ < _)

  println(ex2)
