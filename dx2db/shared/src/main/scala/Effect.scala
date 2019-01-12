package dx2db

import upickle.default.{ReadWriter => RW, macroRW}

sealed trait Effect

object Effect {
  implicit val rw: RW[Effect] = macroRW

  def fromJson(obj: ujson.Obj): Set[Effect] = {
    obj.value.map {
      case ("avoid_death", _) => Tetraja

      case ("repel_phy", _) => Tetrakarn
      case ("repel_mag", _) => Makarakarn
      case ("repel_almighty", _) => FiveElements

      case ("charge_phy", _) => Charge
      case ("charge_mag", _) => Concentrate

      case ("force_critical" | "force_cri", ujson.Bool(true)) => Rebellion

      case ("barrier" | "good_barrier", ujson.Bool(true)) => Barrier
      case ("redia" | "good_redia", ujson.Bool(true)) => Lydia
      case (key, value) => throw new RuntimeException(s"Unknown effect '$key' in '$obj'");
    }.toSet
  }

  @upickle.implicits.key("avoid_death")
  final case object Tetraja extends Effect

  @upickle.implicits.key("repel_phy")
  final case object Tetrakarn extends Effect

  @upickle.implicits.key("repel_mag")
  final case object Makarakarn extends Effect

  @upickle.implicits.key("repel_almighty")
  final case object FiveElements extends Effect

  @upickle.implicits.key("charge_phy")
  final case object Charge extends Effect

  @upickle.implicits.key("concentrate_mag")
  final case object Concentrate extends Effect

  @upickle.implicits.key("force_critical")
  final case object Rebellion extends Effect

  @upickle.implicits.key("barrier")
  final case object Barrier extends Effect

  @upickle.implicits.key("lydia")
  final case object Lydia extends Effect
}
