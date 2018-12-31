package dx2db

// Shift value stored in data3.dvl_skl[n].active.attr.
sealed class Element(val value: Int)

object Element {
  def allElements: Set[Element] = Set(Phys, Fire, Ice, Elec, Force, Light, Dark, Almighty)
  def fromBitmask(value: Int): Set[Element] = {
    allElements.filter(element => (value & (1 << element.value)) != 0)
  }

  final case object Phys extends Element(0)
  final case object Fire extends Element(1)
  final case object Ice extends Element(2)
  final case object Elec extends Element(3)
  final case object Force extends Element(4)
  final case object Light extends Element(5)
  final case object Dark extends Element(6)
  final case object Almighty extends Element(7)
}
