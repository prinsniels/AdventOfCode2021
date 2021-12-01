import scala.io.Source
import java.io.File

extension (fn: String)
  def live = Source
    .fromFile(File(s"src/main/resources/live/${fn}.txt"))
    .getLines

  def tst = Source
    .fromFile(File(s"src/main/resources/test/${fn}.txt"))
    .getLines
