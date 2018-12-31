package dx2db

sealed trait Effect

object Effect {
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

  final case object Tetraja extends Effect

  final case object Tetrakarn extends Effect
  final case object Makarakarn extends Effect
  final case object FiveElements extends Effect

  final case object Charge extends Effect
  final case object Concentrate extends Effect

  final case object Rebellion extends Effect

  final case object Barrier extends Effect
  final case object Lydia extends Effect
}
