package dx2db

import upickle.default._

sealed abstract class Archetype(val stringValue: String)

object Archetype {
  implicit val rw: ReadWriter[Archetype] =
    readwriter[String].bimap[Archetype](_.stringValue, Archetype.fromStringValue(_))

  def all(): Iterable[Archetype] = Seq(Clear, Red, Yellow, Purple, Teal)

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

case class ArchetypeMap[T](clear: Option[T], red: Option[T], yellow: Option[T], purple: Option[T], teal: Option[T]) {
  def values = Seq(clear, red, yellow, purple, teal).flatten

  def apply(archetype: Archetype) = get(archetype).get
  def get(archetype: Archetype) = archetype match {
    case Archetype.Clear => clear
    case Archetype.Red => red
    case Archetype.Yellow => yellow
    case Archetype.Purple => purple
    case Archetype.Teal => teal
  }
}

object ArchetypeMap {
  implicit def rw[T: ReadWriter]: ReadWriter[ArchetypeMap[T]] = {
    readwriter[ujson.Value].bimap[ArchetypeMap[T]](
      map => {
        val obj = ujson.Obj()
        Archetype.all.foreach { archetype =>
          map.get(archetype) match {
            case Some(value) => obj(archetype.stringValue) = writeJs(value)
            case None => {}
          }
        }
        obj
      },
      json => {
        val jsonMap = json.obj.value
        val valueOpts: Iterable[(Archetype, Option[ujson.Value])] = Archetype.all.map(
          archetype => (archetype -> jsonMap.get(archetype.stringValue))
        )

        val values = valueOpts.filter(_._2.isDefined).map {
          case (archetype, opt) => (archetype -> read[T](opt.get))
        }

        ArchetypeMap.fromMap(values.toMap)
      }
    )
  }

  def fromMap[T](map: Map[Archetype, T]) = {
    val clear = map.get(Archetype.Clear)
    val red = map.get(Archetype.Red)
    val yellow = map.get(Archetype.Yellow)
    val purple = map.get(Archetype.Purple)
    val teal = map.get(Archetype.Teal)
    ArchetypeMap(clear, red, yellow, purple, teal)
  }
}
