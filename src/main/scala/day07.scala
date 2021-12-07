import scala.annotation.tailrec

object day07 extends App:

  def optMovement(freq: Map[Int, Int], consumption: Int => Int): (Int, Int) =
    (freq.keys.min to freq.keys.max)
      .map(lvl => {
        lvl -> freq.map((k, v) => consumption(Math.abs(k - lvl)) * v).sum
      })
      .minBy(_._2)

  def incrementedCost(x: Int): Int =
    @tailrec
    def helper(remain: Int, off: Int, cost: Int): Int =
      if (remain == 0) cost
      else helper(remain - 1, off + 1, cost + off)
    helper(x, 1, 0)

  val freq = "day7".live.toList.head.split(',').map(_.toInt).frequency

  optMovement(freq, identity) andThenShowWith "ex1"

  optMovement(freq, incrementedCost) andThenShowWith "ex2"
