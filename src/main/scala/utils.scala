import scala.io.Source
import java.io.File

extension (fn: String)
  def live = Source
    .fromFile(File(s"src/main/resources/live/${fn}.txt"))
    .getLines

  def tst = Source
    .fromFile(File(s"src/main/resources/test/${fn}.txt"))
    .getLines

    
extension [A](ia: List[A])
  def frequency: Map[A, Int] =
    ia.foldLeft(Map.empty[A, Int])((acc, a) => acc + (a -> (acc.getOrElse(a, 0) + 1)))
