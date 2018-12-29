package dx2plan

import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

final case class SkillRow(
  element: String, name: String, jpName: String, cost: String, description: String, target: String, skillPoints: String,
  learnedBy: String, transferableFrom: String
)

case class Skill(name: String, element: String, cost: Option[Int])
object Skill {
  val skills = {
    val parsed = Dx2Db.skills.asCsvReader[SkillRow](rfc.withHeader)
    parsed.map(row => {
      val skill = Skill.fromRow(row.toOption.get)
      (skill.name.toLowerCase, skill)
    }).toMap
  }

  def fromRow(row: SkillRow) = {
    val cost = if (row.cost.endsWith(" MP")) {
      Some(row.cost.stripSuffix(" MP").toInt)
    } else {
      None
    }

    Skill(row.name, row.element, cost)
  }

  def find(name: String): Option[Skill] = {
    if (name.isEmpty) {
      None
    } else {
      skills.get(name.toLowerCase) match {
        case Some(skill) => Some(skill)
        case None => {
          println(s"Failed to find skill '$name'")
          None
        }
      }
    }
  }
}
