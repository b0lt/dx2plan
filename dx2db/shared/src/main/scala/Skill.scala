package dx2db

import scala.io.Source

case class SkillId(value: Int)

sealed trait Skill {
  def id: SkillId
  def name: String
  def description: String
  def rawData: ujson.Obj

  def isActive: Boolean
  final def isPassive: Boolean = !isActive

  def asActive = this.asInstanceOf[Skill.Active]
  def asPassive = this.asInstanceOf[Skill.Passive]

  override def toString() = name
}

object Skill {
  case class Active(id: SkillId, name: String, description: String, rawData: ujson.Obj, element: Option[Element],
                    mpCost: Int, target: Target, effects: Set[Effect], effectCancels: Set[Effect]) extends Skill {
    final override def isActive = true

    def activeRaw = rawData("active").obj
  }

  case class Passive(id: SkillId, name: String, description: String, rawData: ujson.Obj) extends Skill {
    final override def isActive = false
  }
}

case class SkillDb(skills: Map[SkillId, Skill]) {
  lazy val nameToId: Map[String, SkillId] = {
    skills.map {
      case (id, skill) => (skill.name, id)
    }.toMap
  }

  def actives: Iterable[Skill.Active] = filterValues(_.isActive).map(_.asInstanceOf[Skill.Active])
  def passives: Iterable[Skill.Passive] = filterValues(_.isPassive).map(_.asInstanceOf[Skill.Passive])

  def apply(skillName: String) = skills(nameToId(skillName))
  def apply(id: SkillId) = skills(id)

  def filter(f: (Tuple2[SkillId, Skill]) => Boolean): SkillDb = SkillDb(skills.filter(f).toMap)
  def filterKeys(f: SkillId => Boolean): SkillDb = SkillDb(skills.filterKeys(f))
  def filterValues(f: Skill => Boolean): Iterable[Skill] = skills.values.filter(f)
  def groupBy[T](f: Skill => T): Map[T, Iterable[Skill]] = skills.values.groupBy(f)
  def map[T](f: Skill => T): Iterable[T] = skills.values.map(f)
  def flatMap[T](f: Skill => Iterable[T]): Iterable[T] = skills.values.flatMap(f)
}
