package dx2db

case class AltemaId(value: Int)
case class AltemaDemon(id: AltemaId, name: String, stats: Stats) extends StringableKey {
  def asStringKey = name
}

case class AltemaDb(
  backing: Map[AltemaId, AltemaDemon]
) extends TypedMap[AltemaDb, AltemaId, AltemaDemon] with CaseInsensitiveStringMap[AltemaId, AltemaDemon] {
  def construct(map: Map[AltemaId, AltemaDemon]) = AltemaDb(backing)

  def augment(demons: DemonDb): DemonDb = {
    DemonDb(
      demons.map {
        case (demonId, demon) => {
          demon.names.get("jp") match {
            case Some(jpName) => {
              this.get(jpName) match {
                case Some(altemaDemon) => {
                  (demonId -> demon.copy(stats = altemaDemon.stats))
                }

                case None => {
                  System.err.println(s"warning: failed to find Altema entry for ${demon.name}")
                  (demonId -> demon)
                }
              }
            }

            case None => {
              System.err.println(s"warning: failed to find Japanese name for ${demon.name}")
              (demonId -> demon)
            }
          }
        }
      }.toMap
    )
  }
}

object AltemaParser {
  def parseDemon(demonData: ujson.Obj) = {
    val id = AltemaId(demonData("id").num.toInt)
    val name = demonData("name").str
    val stats = Stats(
      hp = demonData("hp").num.toInt,
      strength = demonData("power").num.toInt,
      magic = demonData("mp").num.toInt,
      vitality = demonData("health").num.toInt,
      agility = demonData("speed").num.toInt,
      luck = demonData("luck").num.toInt,
    )
    AltemaDemon(id, name, stats)
  }

  def parse(dataFiles: DataFiles): Option[AltemaDb] = {
    val map = ujson.read(dataFiles.altema).arr.map {
      elem => {
        val demon = parseDemon(elem.obj)
        (demon.id -> demon)
      }
    }.toMap

    Some(AltemaDb(map))
  }
}
