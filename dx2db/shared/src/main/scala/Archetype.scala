package dx2db

import upickle.default._

sealed class Archetype(val stringValue: String)

object Archetype {
  implicit val rw: ReadWriter[Archetype] =
    readwriter[String].bimap[Archetype](_.stringValue, Archetype.fromStringValue(_))

  case object Clear extends Archetype("clear")
  case object Red extends Archetype("red")
  case object Yellow extends Archetype("yellow")
  case object Purple extends Archetype("purple")
  case object Teal extends Archetype("teal")

  def fromStringValue(value: String): Archetype = value match {
    case "clear" => Archetype.Clear
    case "red" => Archetype.Red
    case "yellow" => Archetype.Yellow
    case "purple" => Archetype.Purple
    case "teal" => Archetype.Teal
    case _ => throw new MatchError(s"Invalid archetype value: $value")
  }

  def fromJsonId(id: Int): Archetype = id match {
    case 0 => Archetype.Clear
    case 1 => Archetype.Red
    case 2 => Archetype.Yellow
    case 3 => Archetype.Purple
    case 4 => Archetype.Teal
    case _ => throw new MatchError(s"Invalid archetype value: $id")
  }
}
