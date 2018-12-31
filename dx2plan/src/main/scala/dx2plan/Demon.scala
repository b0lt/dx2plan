package dx2plan

import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

final case class DemonRow(
  name: String, race: String, grade: Int, rarity: Int, phys: String, fire: String, ice: String,
  elec: String, force: String, light: String, dark: String, hp: Int, strength: Int, magic: Int,
  vitality: Int, agility: Int, luck: Int, skill1: String, skill2: String, skill3: String,
  skillClear: String, skillRed: String, skillYellow: String, skillPurple: String, skillTeal: String,
  skillRedGacha: String, skillYellowGacha: String, skillPurpleGacha: String, skillTealGacha: String,
  ai: String, physAtk: Int, physDef: Int, magAtk: Int, magDef: Int
)

sealed trait Stat
object Stat {
  case object Strength extends Stat
  case object Vitality extends Stat
  case object Magic extends Stat
  case object Agility extends Stat
  case object Luck extends Stat
}

sealed trait Color {
  def serialize() = {
    this match {
      case Color.Clear => "Clear"
      case Color.Red => "Red"
      case Color.Yellow => "Yellow"
      case Color.Purple => "Purple"
      case Color.Teal => "Teal"
    }
  }
}

object Color {
  def deserialize(str: String) = {
    str match {
      case "Clear" => Color.Clear
      case "Red" => Color.Red
      case "Yellow" => Color.Yellow
      case "Purple" => Color.Purple
      case "Teal" => Color.Teal
      case _ => {
        println(s"Unknown color '$str'")
        Color.Clear
      }
    }
  }

  case object Clear extends Color
  case object Red extends Color
  case object Yellow extends Color
  case object Purple extends Color
  case object Teal extends Color
}

case class Demon(name: String, stats: Map[Stat, Int], baseSkills: List[Skill],
                 awakenSkills: Map[Color, Option[Skill]], gachaSkills: Map[Color, Option[Skill]])

object Demon {
  private val demons = {
    val parsed = Dx2Db.demons.asCsvReader[DemonRow](rfc.withHeader)
    parsed.map(row => {
      val demon = Demon.fromRow(row.toOption.get)
      (demon.name.toLowerCase(), demon)
    }).toMap
  }

  private def fromRow(row: DemonRow) = {
    val stats = Map[Stat, Int](
      Stat.Strength -> row.strength,
      Stat.Vitality -> row.vitality,
      Stat.Magic -> row.magic,
      Stat.Agility -> row.agility,
      Stat.Luck -> row.luck,
    )

    val baseSkillOpts = List(row.skill1, row.skill2, row.skill3).map(skill => Skill.find(skill))
    val baseSkills = baseSkillOpts.filter(!_.isEmpty).map(_.get)
    val awakenSkills = Map[Color, String](
      Color.Clear -> row.skillClear,
      Color.Red -> row.skillRed,
      Color.Yellow -> row.skillYellow,
      Color.Purple -> row.skillPurple,
      Color.Teal -> row.skillTeal,
    ).mapValues(skill => Skill.find(skill))

    val gachaSkills = Map[Color, String](
      Color.Red -> row.skillRedGacha,
      Color.Yellow -> row.skillYellowGacha,
      Color.Purple -> row.skillPurpleGacha,
      Color.Teal -> row.skillTealGacha,
    ).mapValues(skill => Skill.find(skill))

    Demon(row.name, stats, baseSkills, awakenSkills, gachaSkills)
  }

  def find(name: String): Option[Demon] = {
    demons.get(name.toLowerCase())
  }
}
