package dx2db

import upickle.default._

case class DemonId(value: Int)
case object DemonId {
  implicit val rw: ReadWriter[DemonId] = readwriter[Int].bimap[DemonId](_.value, DemonId(_))
}

case class Demon(
    id: DemonId,
    names: Map[String, String],
    race: Race,
    grade: Int,
    description: String,
    stats: Stats,
    baseSkills: Seq[SkillId],
    archetypeSkills: Map[Archetype, SkillId],
) extends StringableKey {
  def name: String = {
    names.get("en") match {
      case Some(name) => name
      case None => names("jp")
    }
  }

  def obtainableSkills: Set[SkillId] = (baseSkills ++ archetypeSkills.values).toSet

  override def asStringKey = name
}
object Demon {
  implicit val rw: ReadWriter[Demon] = macroRW
}


case class DemonDb(
    demons: Map[DemonId, Demon]
) extends TypedMap[DemonDb, DemonId, Demon] with StringMap[DemonId, Demon] {
  override def backing = demons
  override def construct(map: Map[DemonId, Demon]) = DemonDb(map)
}

object DemonDb {
  implicit val rw: ReadWriter[DemonDb] = readwriter[Iterable[Demon]].bimap[DemonDb](
    _.demons.values,
    demons => {
      val demonMap = demons.map(demon => (demon.id -> demon)).toMap
      DemonDb(demonMap)
    }
  )
}
