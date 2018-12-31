package dx2db

sealed trait Archetype {
}

object Archetype {
  case object Clear extends Archetype
  case object Red extends Archetype
  case object Yellow extends Archetype
  case object Purple extends Archetype
  case object Teal extends Archetype

  def fromJsonId(id: Int): Archetype = {
    id match {
      case 0 => Archetype.Clear
      case 1 => Archetype.Red
      case 2 => Archetype.Yellow
      case 3 => Archetype.Purple
      case 4 => Archetype.Teal
      case _ => throw new MatchError(s"Invalid archetype value: $id")
    }
  }
}
