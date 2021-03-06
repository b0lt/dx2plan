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

  def maxLevel = 1

  def asActive = this.asInstanceOf[Skill.Active]
  def asPassive = this.asInstanceOf[Skill.Passive]

  def cost(level: Int): Option[Int] = None
  def mpRegen(level: Int): Option[Int] = None

  override def toString() = name
  override def asStringKey = name.toLowerCase
}

object Skill {
  implicit val rw: ReadWriter[Skill] = ReadWriter.merge(macroRW[Active], macroRW[Passive])

  @upickle.implicits.key("active")
  final case class Active(
      id: SkillId,
      name: String,
      description: String,
      element: Option[Element],
      mpCost: Int,
      target: Target,
      effects: Set[Effect],
      effectCancels: Set[Effect],
      attributes: Seq[SkillAttribute],
      levels: Seq[Seq[SkillLevelEffect]],
  ) extends Skill {
    final override def isActive = true
    final override def maxLevel = levels.length + 1
    final override def cost(level: Int) = {
      if (level == 1) {
        Some(mpCost)
      } else {
        val levelEffects = levels(level - 2)
        val reduction = levelEffects.collectFirst {
          case SkillLevelEffect.CostReduction(mp) => mp
        }.getOrElse(0)
        Some(mpCost - reduction)
      }
    }

    final override def mpRegen(level: Int) = {
      if (level == 1) {
        Some(0)
      } else {
        val levelEffects = levels(level - 2)
        val (rate, amount) = levelEffects.collectFirst {
          case SkillLevelEffect.ManaRecovery(rate, amount) => (rate, amount)
        }.getOrElse((0, 0))

        if (rate == 100) {
          Some(amount)
        } else {
          Some(0)
        }
      }
    }

  }

  @upickle.implicits.key("passive")
  final case class Passive(
      id: SkillId,
      name: String,
      description: String,
  ) extends Skill {
    final override def isActive = false
  }
}

sealed trait SkillLevelEffect;

object SkillLevelEffect {
  implicit val rw: ReadWriter[SkillLevelEffect] =
    ReadWriter.merge(
      macroRW[Damage],
      macroRW[ManaRecovery],
      macroRW[AilmentRate],
      macroRW[HealingAmount],
      macroRW[HitRate],
      macroRW[CostReduction],
    )

  @upickle.implicits.key("damage")
  final case class Damage(percentage: Int) extends SkillLevelEffect

  @upickle.implicits.key("mana_recovery")
  final case class ManaRecovery(percentage: Int, mp: Int) extends SkillLevelEffect

  @upickle.implicits.key("ailment_rate")
  final case class AilmentRate(percentage: Int) extends SkillLevelEffect

  @upickle.implicits.key("healing_amount")
  final case class HealingAmount(percentage: Int) extends SkillLevelEffect

  @upickle.implicits.key("hit_rate")
  final case class HitRate(percentage: Int) extends SkillLevelEffect

  @upickle.implicits.key("cost_reduction")
  final case class CostReduction(mp: Int) extends SkillLevelEffect
}

sealed trait SkillAttribute

object SkillAttribute {
  implicit val rw: ReadWriter[SkillAttribute] =
    ReadWriter.merge(
      macroRW[Lifesteal],
      macroRW[ManaDrain],
      macroRW[Instakill],
      macroRW[PressTurnGain],
      macroRW[SelfDamage],
      macroRW[ManaBurn],
    )

  @upickle.implicits.key("lifesteal")
  final case class Lifesteal(percentage: Int) extends SkillAttribute

  @upickle.implicits.key("mana_drain")
  final case class ManaDrain(amount: Int) extends SkillAttribute

  @upickle.implicits.key("instakill")
  final case class Instakill(percentage: Int) extends SkillAttribute

  @upickle.implicits.key("press_turn_gain")
  final case class PressTurnGain(amount: Int) extends SkillAttribute

  @upickle.implicits.key("self_damage")
  final case class SelfDamage(percentage: Int) extends SkillAttribute

  @upickle.implicits.key("mana_burn")
  final case class ManaBurn(amount: Int) extends SkillAttribute
}

case class SkillDb(
    skills: Map[SkillId, Skill]
) extends TypedMap[SkillDb, SkillId, Skill] with CaseInsensitiveStringMap[SkillId, Skill] {
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
