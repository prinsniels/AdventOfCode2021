object day08 extends App:

  case class Line(input: Vector[Set[Char]], output: Vector[Set[Char]])

  val split = raw"(\D+)\|(\D+)" r
  val token = raw"([a-z]+)" r

  val collection: String => Vector[Set[Char]] = i =>
    token.findAllIn(i).map(_.toSet).toVector

  def parse(i: String): Line = i match
    case split(l, r) => Line(collection(l), collection(r))

  def inputToDigits(input: Vector[Set[Char]]): Map[Set[Char], Int] =
    lazy val zero =
      input.filter(_.size == 3).head -- input.filter(_.size == 2).head
    lazy val one = (input.filter(_.size == 6) ++ input.filter(_.size == 4))
      .reduce(_.intersect(_)) -- input.filter(_.size == 2).head
    lazy val two = input.filter(_.size == 2).head -- five
    lazy val three = (input.filter(_.size == 5) ++ input.filter(_.size == 4))
      .reduce(_.intersect(_))
    lazy val four =
      "abcdefg".toSet -- zero -- one -- two -- three -- five -- six
    lazy val five = (input.filter(_.size == 6) ++ input.filter(_.size == 2))
      .reduce(_.intersect(_))
    lazy val six =
      input.filter(_.size == 5).reduce(_.intersect(_)) -- zero -- three

    Map(
      (zero | one | two | four | five | six) -> 0,
      (two | five) -> 1,
      (zero | two | three | four | six) -> 2,
      (zero | two | three | five | six) -> 3,
      (one | two | three | five) -> 4,
      (zero | one | three | five | six) -> 5,
      (zero | one | three | four | five | six) -> 6,
      (zero | two | five) -> 7,
      (zero | one | two | three | four | five | six) -> 8,
      (zero | one | two | three | five | six) -> 9
    )

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
