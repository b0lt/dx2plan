package dx2db

sealed class Target(value: Int)

object Target {
  def fromValue(value: Int): Target = value match {
    case 0 => SingleEnemy
    case 1 => AllEnemies
    case 2 => RandomEnemies
    case 3 => SingleAlly
    case 4 => AllAllies
    case 5 => RandomAllies
    case 6 => Self
    case _ => throw new IllegalArgumentException("Invalid target value " + value)
  }

  final case object SingleEnemy extends Target(0)
  final case object AllEnemies extends Target(1)
  final case object RandomEnemies extends Target(2)
  final case object SingleAlly extends Target(3)
  final case object AllAllies extends Target(4)
  final case object RandomAllies extends Target(5)
  final case object Self extends Target(6)

  final case object Everyone extends Target(-1)
}
