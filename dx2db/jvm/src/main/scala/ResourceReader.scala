package dx2db

import scala.io.Source

object ResourceReader {
  def read(resourceName: String): Option[String] = {
    Some(Source.fromResource(resourceName).getLines.mkString("\n"))
  }
}
