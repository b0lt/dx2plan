package dx2db

case class DemonId(value: Int)



case class Demon(id: DemonId, name: String, jpName: Option[String], race: String, grade: Int, description: String, 
                 baseSkills: Seq[SkillId], archetypeSkills: Map[Archetype, SkillId], rawData: ujson.Obj) {
  override def toString() = name
}

case class DemonDb(demons: Map[DemonId, Demon]) {
  lazy val nameToId: Map[String, DemonId] = {
    demons.map {
      case (id, demon) => (demon.name, id)
    }.toMap
  }

  def apply(demonName: String) = demons(nameToId(demonName))
  def apply(id: DemonId) = demons(id)

  def filter(f: (Tuple2[DemonId, Demon]) => Boolean): DemonDb = DemonDb(demons.filter(f).toMap)
  def filterKeys(f: DemonId => Boolean): DemonDb = DemonDb(demons.filterKeys(f))
  def filterValues(f: Demon => Boolean): Iterable[Demon] = demons.values.filter(f)
  def groupBy[T](f: Demon => T): Map[T, Iterable[Demon]] = demons.values.groupBy(f)
  def map[T](f: Demon => T): Iterable[T] = demons.values.map(f)
  def flatMap[T](f: Demon => Iterable[T]): Iterable[T] = demons.values.flatMap(f)
}
