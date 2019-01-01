package dx2db

import upickle.default._

sealed class Target(intValue: Int, val stringValue: String)

object Target {
  implicit val rw: ReadWriter[Target] = {
    readwriter[String].bimap[Target](_.stringValue, Target.fromStringValue(_))
  }

  def fromIntValue(value: Int): Target = value match {
    case 0 => SingleEnemy
    case 1 => AllEnemies
    case 2 => RandomEnemies
    case 3 => SingleAlly
    case 4 => AllAllies
    case 5 => RandomAllies
    case 6 => Self
    case _ => throw new IllegalArgumentException("Invalid target value " + value)
  }

  def fromStringValue(value: String): Target = value match {
    case "enemy" => SingleEnemy
    case "all_enemies" => AllEnemies
    case "random_enemies" => RandomEnemies
    case "ally" => SingleAlly
    case "all_allies" => AllAllies
    case "random_allies" => RandomAllies
    case "self" => Self
    case "everyone" => Everyone
    case _ => throw new IllegalArgumentException("Invalid target value " + value)
  }

  final case object SingleEnemy extends Target(0, "enemy")
  final case object AllEnemies extends Target(1, "all_enemies")
  final case object RandomEnemies extends Target(2, "random_enemies")
  final case object SingleAlly extends Target(3, "ally")
  final case object AllAllies extends Target(4, "all_allies")
  final case object RandomAllies extends Target(5, "random_allies")
  final case object Self extends Target(6, "self")

  final case object Everyone extends Target(-1, "everyone")
}
