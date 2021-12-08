object day08 extends App:

  case class Line(input: Vector[Set[Char]], output: Vector[Set[Char]])

  val split = raw"(\D+)\|(\D+)" r
  val token = raw"([a-z]+)" r

  val collection: String => Vector[Set[Char]] = i =>
    token.findAllIn(i).map(_.toSet).toVector

  def parse(i: String): Line = i match
    case split(l, r) => Line(collection(l), collection(r))

  def inputToDigits(input: Vector[Set[Char]]): Map[Set[Char], Int] =
    input.map { i =>
      i match {
        case i if i.size == 2 => i -> 1
        case i if i.size == 3 => i -> 7
        case i if i.size == 4 => i -> 4
        case i
            if i.size == 5 & i
              .intersect(input.find(_.size == 2).get)
              .size == 2 =>
          i -> 3
        case i
            if i.size == 5 & i
              .intersect(input.find(_.size == 4).get)
              .size == 2 =>
          i -> 2
        case i if i.size == 5 => i -> 5
        case i
            if i.size == 6 & i
              .intersect(input.find(_.size == 2).get)
              .size == 1 =>
          i -> 6
        case i
            if i.size == 6 & i
              .intersect(input.find(_.size == 4).get)
              .size == 4 =>
          i -> 9
        case i if i.size == 6 => i -> 0
        case _                => i -> 8
      }
    }.toMap

  "day8".live
    .map(parse)
    .map(_.output)
    .map(x => x.filter(y => Set(2, 3, 4, 7)(y.size)).size)
    .toList
    .sum andThenShowWith "ex1"

  "day8".live
    .map(parse)
    .map(line => {
      val digitMap = inputToDigits(line.input)
      line.output.map(digitMap).mkString.toInt
    })
    .sum andThenShowWith "ex2"
