package dx2plan

import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

final case class DemonRow(
  name: String, race: String, grade: Int, rarity: Int, hp: String, strength: String, magic: String,
  vitality: String, agility: String, luck: String, phys: String, fire: String, ice: String,
  elec: String, force: String, light: String, dark: String, skill1: String, skill2: String,
  skill3: String, skillClear: String, skillRed: String, skillYellow: String, skillPurple: String,
  skillTeal: String, ai: String, physAtk: String, physDef: String, magAtk: String, magDef: String
)

sealed trait Stat
object Stat {
  case object Strength extends Stat
  case object Vitality extends Stat
  case object Magic extends Stat
  case object Agility extends Stat
  case object Luck extends Stat
}

sealed trait Color
object Color {
  case object Clear extends Color
  case object Red extends Color
  case object Yellow extends Color
  case object Purple extends Color
  case object Teal extends Color
}

case class Demon(name: String, stats: Map[Stat, Int], baseSkills: List[Skill], awakenSkills: Map[Color, Option[Skill]])

object Demon {
  private val demons = {
    val parsed = Dx2Db.demons.asCsvReader[DemonRow](rfc.withHeader)
    parsed.map(row => {
      val demon = Demon.fromRow(row.toOption.get)
      (demon.name.toLowerCase(), demon)
    }).toMap
  }

  private def parseSkillName(skill: String) = {
    skill.substring(0, skill.indexOf('|'))
  }

  private def fromRow(row: DemonRow) = {
    val stats = Map[Stat, String](
      Stat.Strength -> row.strength,
      Stat.Vitality -> row.vitality,
      Stat.Magic -> row.magic,
      Stat.Agility -> row.agility,
      Stat.Luck -> row.luck,
    ).mapValues(stat => {
      // The stat values are of the format "actual_value (rank/number of demons)", which is unhelpful.
      stat.split(" ")(0).toInt
    })

    val baseSkillOpts = List(row.skill1, row.skill2, row.skill3).map(skill => Skill.find(parseSkillName(skill)))
    val baseSkills = baseSkillOpts.filter(!_.isEmpty).map(_.get)
    val awakenSkills = Map[Color, String](
      Color.Clear -> row.skillClear,
      Color.Red -> row.skillRed,
      Color.Yellow -> row.skillYellow,
      Color.Purple -> row.skillPurple,
      Color.Teal -> row.skillTeal,
    ).mapValues(skill => Skill.find(parseSkillName(skill)))
    Demon(row.name, stats, baseSkills, awakenSkills)
  }

  def find(name: String): Option[Demon] = {
    demons.get(name.toLowerCase())
  }
}
