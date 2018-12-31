package dx2db

case class DemonId(value: Int)

case class Demon(
    id: DemonId,
    name: String,
    jpName: Option[String],
    race: String,
    grade: Int,
    description: String,
    baseSkills: Seq[SkillId],
    archetypeSkills: Map[Archetype, SkillId],
    rawData: ujson.Obj
) extends StringableKey {
  override def toString() = name
  override def asStringKey = name
}

case class DemonDb(
    demons: Map[DemonId, Demon]
) extends TypedMap[DemonDb, DemonId, Demon] with StringMap[DemonId, Demon] {
  override def backing = demons
  override def construct(map: Map[DemonId, Demon]) = DemonDb(map)
}
