
object day10 extends App:

  val charCombinations = Map[Char, Char]('[' -> ']', '{' -> '}', '<' -> '>', '(' -> ')')

  val pointsEx1 = Map(')' -> 3L, ']' -> 57L, '}' -> 1197L, '>' -> 25137L)

  def firstCorrupted(line: List[Char]): Option[(Char, Char)] =

    def helper(
        remain: List[Char],
        expectation: List[Char]
    ): Option[(Char, Char)] =
      remain match
        case x :: xs if charCombinations.keySet(x) => helper(xs, charCombinations(x) :: expectation)
        case x :: xs =>
          if (x == expectation.head) helper(xs, expectation.tail)
          else Some(expectation.head, x)
        case Nil => None

    helper(line, List.empty[Char])

  val pointsEx2 = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

  val lineScore: List[Int] => Long = l => l.foldLeft(0L)((t, i) => t * 5L + i)

  def remaining(line: List[Char]): List[Char] =
    def helper(remain: List[Char], expectation: List[Char]): List[Char] =
      remain match
        case x :: xs if charCombinations.keySet(x) => helper(xs, charCombinations(x) :: expectation)
        case x :: xs                    => helper(xs, expectation.tail)
        case _                          => expectation
    helper(line, List.empty[Char])

  "day10".tst
    .map(_.toList)
    .map(firstCorrupted)
    .collect({ case Some(a, b) => b })
    .map(pointsEx1)
    .sum andThenShowWith "ex1"

  val scores = "day10".live
    .map(_.toList)
    .filter({ x =>
      firstCorrupted(x) match
        case Some(_) => false
        case None    => true
    })
    .map(remaining)
    .map(_.map(pointsEx2))
    .map(lineScore)
    .toList.sorted

  scores.drop(scores.size / 2).head andThenShowWith "ex2"
