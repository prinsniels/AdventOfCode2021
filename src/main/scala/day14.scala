object day14 extends App:

  val parseInsertion: String => (String, String) = line =>
    line match
      case s"${a} -> ${b}" => a -> b


  def expand(pair: String, table: Map[String, String]): List[String] =
    val insertion = table.getOrElse(pair, "-")
    List(pair(0).toString + insertion, insertion + pair(1).toString)

  
  def transform(initial: Map[String, Long]): Map[String, Long] =
    initial.map((k, v) => expand(k, insert).map(_ -> v).toMap).reduce(Utils.mapSumLong)

  val (baseRaw, insertionsRaw) = "day14".live.splitAt(1)
  val startString = baseRaw.toList.head
  val insert: Map[String, String] = insertionsRaw.drop(1).map(x => parseInsertion(x)).toList.toMap

  val initial = startString.sliding(2, 1).toList.frequency.map((k, v) => k -> v.toLong)

  lazy val transformed = (1 to 40).foldLeft(initial)((acc, _) => transform(acc))

  lazy val letterFrequency = transformed.toList
    .flatMap((k, v) => k.map(_.toString -> v))
    .foldLeft(Map.empty[String, Long])((l, r) => l + (r._1 -> (l.getOrElse(r._1, 0L) + r._2)))
    .map((k, v) => k -> math.ceil(v / 2.0).toLong)

  letterFrequency.maxBy(_._2)._2 - letterFrequency.minBy(_._2)._2 andThenShowWith "ex"
