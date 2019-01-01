package dx2db

import scala.io.Source

case class LocalizedDataFiles(dataFile1: String, dataFile2: String, dataFile3: String)
case class DataFiles(en: LocalizedDataFiles, jp: LocalizedDataFiles, altema: String)

object DataFiles {
  def fromResources(): Option[DataFiles] = {
    val en = Seq("en/data1", "en/data2", "en/data3").map(ResourceReader.read).flatten
    val jp = Seq("jp/data1", "jp/data2", "jp/data3").map(ResourceReader.read).flatten
    if (en.length == 3 && jp.length == 3) {
      ResourceReader.read("altema.json") map { altema =>
        DataFiles(
          LocalizedDataFiles(en(0), en(1), en(2)),
          LocalizedDataFiles(jp(0), jp(1), jp(2)),
          altema
        )
      }
    } else {
      None
    }
  }
}
