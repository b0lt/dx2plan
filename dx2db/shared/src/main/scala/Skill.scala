package dx2db

import scala.io.Source

case class SkillId(value: Int)

sealed trait Skill extends StringableKey {
  def id: SkillId
  def name: String
  def description: String

  def isActive: Boolean
  final def isPassive: Boolean = !isActive

  def asActive = this.asInstanceOf[Skill.Active]
  def asPassive = this.asInstanceOf[Skill.Passive]

  override def toString() = name
  override def asStringKey = name
}

object Skill {
  case class Active(
      id: SkillId,
      name: String,
      description: String,
      element: Option[Element],
      mpCost: Int,
      target: Target,
      effects: Set[Effect],
      effectCancels: Set[Effect]
  ) extends Skill {
    final override def isActive = true
  }

  case class Passive(
      id: SkillId,
      name: String,
      description: String,
  ) extends Skill {
    final override def isActive = false
  }
}

case class SkillDb(
    skills: Map[SkillId, Skill]
) extends TypedMap[SkillDb, SkillId, Skill] with StringMap[SkillId, Skill] {
  override def backing = skills
  override def construct(map: Map[SkillId, Skill]) = SkillDb(map)

  def actives: Iterable[Skill.Active] = filterValues(_.isActive).map(_.asInstanceOf[Skill.Active])
  def passives: Iterable[Skill.Passive] = filterValues(_.isPassive).map(_.asInstanceOf[Skill.Passive])
}
