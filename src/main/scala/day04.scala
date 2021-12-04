case class Card(rows: List[Set[Int]])

object day04 extends App:

  def parseRow(row: String): List[Int] =
    val pattern = raw"(\d+)".r
    pattern.findAllMatchIn(row).map(_.toString.toInt).toList

  def getCards(input: Iterator[String]): List[Card] =
    input.sliding(6, 6).foldLeft(List.empty[Card]) { case (cards, remain) =>
      val raw = remain.drop(1).map(parseRow).toList
      Card(raw.map(_.toSet) ++ raw.transpose.map(_.toSet)) :: cards
    }

  def isWinner(inputs: Set[Int], card: Card): Boolean =
    card.rows.find(_.subsetOf(inputs)) match
      case None    => false
      case Some(_) => true

  def cardScore(card: Card, taken: Set[Int], last: Int) =
    (card.rows.foldLeft(Set.empty[Int])(_ ++ _) -- taken).sum * last

  def findWinner(
      remain: List[Int],
      cards: List[Card],
      taken: Set[Int]
  ): Option[(Card, Set[Int], Int)] =
    remain match
      case hd :: tail => {
        cards.find(c => isWinner(taken + hd, c)) match
          case Some(card) => Some((card, taken + hd, hd))
          case None       => findWinner(tail, cards, taken + hd)
      }
      case Nil => None

  def worstPlay(inputs: List[Int], cards: List[Card]): (Card, Set[Int], Int) =
    cards
      .map(card => findWinner(input, List(card), Set.empty[Int]))
      .collect { case Some(r) => r._2.size -> r }
      .maxBy(_._1)
      ._2

  val src = "day4".live
  val input = src.next.split(',').map(_.toInt).toList

  val cards = getCards(src)

  findWinner(input, cards, Set.empty[Int]).fold(println("nothing Found")) {
    case (card, taken, last) => println(cardScore(card, taken, last))
  }

  val (card, taken, last) = worstPlay(input, cards)
  println(cardScore(card, taken, last))
