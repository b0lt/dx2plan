package dx2db

import scala.io.Source

import upickle.default._

case class SkillId(value: Int)
case object SkillId {
  implicit val rw: ReadWriter[SkillId] = readwriter[Int].bimap[SkillId](_.value, SkillId(_))
}

sealed trait Skill extends StringableKey {
  def id: SkillId
  def name: String
  def description: String

  def isActive: Boolean
  final def isPassive: Boolean = !isActive

  def asActive = this.asInstanceOf[Skill.Active]
  def asPassive = this.asInstanceOf[Skill.Passive]

  def cost: Option[Int] = this match {
    case active: Skill.Active => Some(active.mpCost)
    case passive: Skill.Passive => None
  }

  override def toString() = name
  override def asStringKey = name
}

object Skill {
  implicit val rw: ReadWriter[Skill] = ReadWriter.merge(Active.rw, Passive.rw)

  @upickle.implicits.key("active")
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
  object Active {
    implicit val rw: ReadWriter[Active] = macroRW
  }

  @upickle.implicits.key("passive")
  case class Passive(
      id: SkillId,
      name: String,
      description: String,
  ) extends Skill {
    final override def isActive = false
  }
  object Passive {
    implicit val rw: ReadWriter[Passive] = macroRW
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

object SkillDb {
  implicit val rw: ReadWriter[SkillDb] = readwriter[Iterable[Skill]].bimap[SkillDb](
    _.skills.values,
    skills => {
      val skillMap = skills.map(skill => (skill.id -> skill)).toMap
      SkillDb(skillMap)
    }
  )
}
