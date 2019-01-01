package dx2db

case class DemonId(value: Int)

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

  override def asStringKey = name
}

case class DemonDb(
    demons: Map[DemonId, Demon]
) extends TypedMap[DemonDb, DemonId, Demon] with StringMap[DemonId, Demon] {
  override def backing = demons
  override def construct(map: Map[DemonId, Demon]) = DemonDb(map)
}
