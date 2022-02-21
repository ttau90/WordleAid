package Utils

import scala.util.Using
import scala.util.{Try, Success, Failure}

object FileHelperUtil {
  def readFileToSeq(filePath: String): Try[Seq[String]] = {
    Using(io.Source.fromResource(filePath)) { bufferedSource =>
      bufferedSource.getLines.toSeq
    }
  }
}
