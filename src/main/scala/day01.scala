object Day1 extends App:

  val lines = "day1".live
    .map(_.toInt)
    .toList

  lines.zip(lines.tail).count(_ < _) andThenShowWith "ex1"

  val sliding = lines.sliding(3).map(_.sum).toList

  sliding.zip(sliding.tail).count(_ < _) andThenShowWith "ex2"
