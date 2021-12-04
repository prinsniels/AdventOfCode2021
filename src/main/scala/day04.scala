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

  def hasBingo(inputs: Set[Int], card: Card): Boolean =
    card.rows.find(_.subsetOf(inputs)) match
      case None    => false
      case Some(_) => true

  def cardScore(card: Card, taken: Set[Int], last: Int) =
    (card.rows.foldLeft(Set.empty[Int])(_ ++ _) -- taken).sum * last

  def playCard(remain: List[Int], card: Card, taken: Set[Int]): Option[(Set[Int], Int)] =
    remain match
      case hd :: tail => 
          if (hasBingo(taken + hd, card)) Some(taken + hd, hd)
          else playCard(tail, card, taken + hd)
      case Nil => None

  val src = "day4".live
  val input = src.next.split(',').map(_.toInt).toList
  val cards = getCards(src)

  val turnsAndScore = cards
  .map(card => playCard(input, card, Set.empty[Int]).map( (used, last) => used.size -> cardScore(card, used, last)))
  .collect({case Some(turns -> score) => turns -> score})

  println(s"best ${turnsAndScore.minBy(_._1)._2}")
  println(s"worst ${turnsAndScore.maxBy(_._1)._2}")