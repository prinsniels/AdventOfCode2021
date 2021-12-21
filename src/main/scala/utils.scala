import scala.io.Source
import java.io.File

extension (fn: String)
  def live = Source
    .fromFile(File(s"src/main/resources/live/${fn}.txt"))
    .getLines

  def tst = Source
    .fromFile(File(s"src/main/resources/test/${fn}.txt"))
    .getLines

    
extension [A](ia: Iterable[A])
  def frequency: Map[A, Int] =
    ia.foldLeft(Map.empty[A, Int])((acc, a) => acc + (a -> (acc.getOrElse(a, 0) + 1)))


extension [A](a: A)
  def andThenShow: A = 
    println(a)
    a

  def andThenShowWith(w: String): A = 
    println(s"${w}:${a}") 
    a

object Utils:    
  def mapSumInt[A](l: Map[A, Int], r: Map[A, Int]): Map[A, Int] =
    l ++ r.map((k, v) => k -> (l.getOrElse(k, 0) + v))

  def mapSumLong[A](l: Map[A, Long], r: Map[A, Long]): Map[A, Long] =
    l ++ r.map((k, v) => k -> (l.getOrElse(k, 0L) + v))