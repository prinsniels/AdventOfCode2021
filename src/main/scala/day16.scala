object day16 extends App:

  def hexToBitString(hex: String): String =
    /** Long.parseLong(...).toBinaryString) did not work as expected given mapping is probably different than standard
      * lib
      */
    val dict = Map(
      '0' -> "0000",
      '1' -> "0001",
      '2' -> "0010",
      '3' -> "0011",
      '4' -> "0100",
      '5' -> "0101",
      '6' -> "0110",
      '7' -> "0111",
      '8' -> "1000",
      '9' -> "1001",
      'A' -> "1010",
      'B' -> "1011",
      'C' -> "1100",
      'D' -> "1101",
      'E' -> "1110",
      'F' -> "1111"
    )
    hex.map(dict).mkString

  def bitStringToInt(s: String): Int =
    Integer.parseInt(s, 2)

  def bitStringToLong(s: String): Long =
    java.lang.Long.parseLong(s, 2)
  
  def identify(bitString: String): (Int, Int, String) =
    val (v, r1) = bitString.splitAt(3)
    val (t, r2) = r1.splitAt(3)
    (bitStringToInt(v), bitStringToInt(t), r2)

  def consumeUntil(i: List[String], pred: String => Boolean): List[String] = i match
    case Nil                => Nil
    case x :: xs if pred(x) => x :: Nil
    case x :: xs            => x :: consumeUntil(xs, pred)

  trait Packet
  case class Literal(ver: Int, v: Long) extends Packet
  case class Operator(ver: Int, t: Int, vs: List[Packet]) extends Packet

  def retrieveLiteralPayload(version: Int, payload: String): (Literal, String) =
    val chunks = consumeUntil(payload.sliding(5, 5).toList, _.startsWith("0"))
    Literal(version, bitStringToLong(chunks.map(_.tail).mkString)) -> payload.drop(chunks.size * 5)

  def untilWithMaxLength(remain: String, acc: List[Packet]): List[Packet] =
    if (remain.length <= 10) acc.reverse
    else {
      val (p, r) = decode(remain)
      untilWithMaxLength(r, p :: acc)
    }

  def untilWithRequiredAmount(remain: String, acc: List[Packet], needed: Int): (List[Packet], String) =
    if (needed <= 0) acc.reverse -> remain
    else {
      val (p, r) = decode(remain)
      untilWithRequiredAmount(r, p :: acc, needed - 1)
    }

  def decode(s: String): (Packet, String) =
    identify(s) match
      case (v, 4, data) => retrieveLiteralPayload(v, data)
      case (v, t, data) if data.head == '0' => {
        val (size, pData) = data.tail.splitAt(15)
        val (inPackage, remain) = pData.splitAt(bitStringToInt(size))
        Operator(v, t, untilWithMaxLength(inPackage, List.empty)) -> remain
      }
      case (v, t, data) if data.head == '1' => {
        val (nrPackages, pData) = data.tail.splitAt(11)
        val requiredPackages = bitStringToInt(nrPackages)
        val (ps, r) = untilWithRequiredAmount(pData, List.empty, requiredPackages)
        Operator(v, t, ps) -> r
      }

  def sumVersions(p: Packet): Int =
    p match
      case a: Operator => a.ver + a.vs.map(sumVersions).sum
      case a: Literal  => a.ver

  def eval(p: Packet): Long =
    p match
      case Operator(v, 0, vs) => vs.foldLeft(0L)((acc, p) => acc + eval(p))
      case Operator(v, 1, vs) => vs.foldLeft(1L)((acc, p) => acc * eval(p))
      case Operator(v, 2, vs) => vs.map(eval).min
      case Operator(v, 3, vs) => vs.map(eval).max
      case Operator(v, 5, vs) => if (eval(vs.head) > eval(vs.tail.head)) 1 else 0
      case Operator(v, 6, vs) => if (eval(vs.head) < eval(vs.tail.head)) 1 else 0
      case Operator(v, 7, vs) => if (eval(vs.head) == eval(vs.tail.head)) 1 else 0
      case a: Literal         => a.v

  sumVersions(decode(hexToBitString("day16".live.toList.head))._1) andThenShowWith "ex1"

  eval(decode(hexToBitString("day16".live.toList.head))._1) andThenShowWith "ex2"
