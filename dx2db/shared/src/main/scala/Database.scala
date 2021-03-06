package dx2db

import upickle.default.{ReadWriter => RW, macroRW}

case class Database(demons: DemonDb, skills: SkillDb)
object Database {
  implicit val rw: RW[Database] = macroRW
  def fromUrl(url: String): Option[Database] = {
    DatabaseFetcher.fetch(url)
  }
}
