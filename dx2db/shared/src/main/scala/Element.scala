package dx2db

import upickle.default._

// Shift value stored in data3.dvl_skl[n].active.attr.
sealed class Element(val intValue: Int, val stringValue: String)

object Element {
  implicit val rw: ReadWriter[Element] = {
    readwriter[String].bimap[Element](_.stringValue, Element.fromStringValue(_))
  }

  def allElements: Set[Element] = Set(Phys, Fire, Ice, Elec, Force, Light, Dark, Almighty)
  def fromBitmask(intValue: Int): Set[Element] = {
    allElements.filter(element => (intValue & (1 << element.intValue)) != 0)
  }

  def fromStringValue(elem: String): Element = {
    elem match {
      case "phys" => Phys
      case "fire" => Fire
      case "ice" => Ice
      case "elec" => Elec
      case "force" => Force
      case "light" => Light
      case "dark" => Dark
      case "almighty" => Almighty
      case _ => throw new RuntimeException(s"Invalid Element '$elem'")
    }
  }

  final case object Phys extends Element(0, "phys")
  final case object Fire extends Element(1, "fire")
  final case object Ice extends Element(2, "ice")
  final case object Elec extends Element(3, "elec")
  final case object Force extends Element(4, "force")
  final case object Light extends Element(5, "light")
  final case object Dark extends Element(6, "dark")
  final case object Almighty extends Element(7, "almighty")
}
