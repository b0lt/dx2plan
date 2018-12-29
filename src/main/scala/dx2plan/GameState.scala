package dx2plan

sealed trait Move {
  def name: String
  def element: String
  def mpCost: Int
  def pressTurnCost: Double

  final def serialize() = {
    this match {
      case Pass => "Pass"
      case Attack => "Attack"
      case Spell(skill, awakened) => {
        if (awakened) {
          s"${skill.name} (awakened)"
        } else {
          skill.name
        }
      }
    }
  }
}

case object Pass extends Move {
  def name = "Pass"
  def element = "none"
  def mpCost = 0
  def pressTurnCost = 0.5
}

case object Attack extends Move {
  def name = "Attack"
  def element = "phys"
  def mpCost = 0
  def pressTurnCost = 1
}

case class Spell(skill: Skill, awakened: Boolean) extends Move {
  def name = skill.name
  def element = skill.element
  def mpCost = {
    if (awakened) {
      skill.cost.getOrElse(1) - 1
    } else {
      skill.cost.getOrElse(0)
    }
  }

  def pressTurnCost = {
    name match {
      case "Tag" => 0
      case _ => 1
    }
  }
}

sealed trait GlobalModifier
case object OrleansPrayer extends GlobalModifier

sealed trait DemonModifier {
  def consumedBy(element: String): Boolean

}

object DemonModifier {
  def isMagicalNuke(element: String) = {
    element match {
      case "almighty" | "fire" | "ice" | "elec" | "force" | "light" | "dark" => true
      case _ => false
    }
  }
}

case object Rebellion extends DemonModifier {
  def consumedBy(element: String) = element == "phys"
}

case object Charge extends DemonModifier {
  def consumedBy(element: String) = element == "phys"
}

case object Concentrate extends DemonModifier {
  def consumedBy(element: String) = DemonModifier.isMagicalNuke(element)
}

object Move {
  def deserialize(string: String): Move = {
    val awakened = raw"(.+) \(awakened\)".r
    string match {
      case "Pass" => Pass
      case "Attack" => Attack
      case awakened(name) => Skill.find(name) match {
        case Some(skill) => Spell(skill, true)
        case None => Pass
      }
      case unawakened => Skill.find(unawakened) match {
        case Some(skill) => Spell(skill, false)
        case None => Pass
      }
    }
  }
}

case class GameState(turnNumber: Int, pressTurns: Double, demonMp: Map[DemonId, Int],
                     globalModifiers: Set[GlobalModifier], demonModifiers: Map[DemonId, Set[DemonModifier]]) {
  def evaluate(demonId: DemonId, move: Move) = {
    var newDemonModifiers = demonModifiers
    var newGlobalModifiers = globalModifiers
    var pressTurnCost = move.pressTurnCost

    if (move.element == "phys" && demonModifiers(demonId).contains(Rebellion)) {
      pressTurnCost = 0.5
    }

    move.name match {
      case "Charge" => {
        newDemonModifiers += (demonId -> (demonModifiers(demonId) + Charge - Concentrate))
      }

      case "Concentrate" => {
        newDemonModifiers += (demonId -> (demonModifiers(demonId) + Concentrate - Charge))
      }

      case "Rebellion" => {
        newDemonModifiers += (demonId -> (demonModifiers(demonId) + Rebellion))
      }

      case "Mega Boost" => {
        newDemonModifiers += (demonId -> (demonModifiers(demonId) + Charge + Rebellion))
      }

      case "Red Zone" => {
        for (i <- 0 until 4) {
          val id = DemonId(i)
          newDemonModifiers += (id -> (demonModifiers(id) + Rebellion))
        }
      }

      case "Orleans Prayer" => {
        newGlobalModifiers += OrleansPrayer
      }

      case "Silent Prayer" => {
        newDemonModifiers = demonModifiers.map { case (id, _) => id -> Set[DemonModifier]() }.toMap
      }

      case _ => {}
    }

    newDemonModifiers =
      newDemonModifiers +
      (demonId -> newDemonModifiers(demonId).filterNot(_.consumedBy(move.element)))

    val newPressTurns = {
      if (pressTurnCost == 0) {
        pressTurns
      } else if (pressTurns % 1 == 0) {
        pressTurns - pressTurnCost
      } else {
        pressTurns - 0.5
      }
    }

    val newDemonMp = {
      val mp = demonMp(demonId) - move.mpCost
      if (mp < 0 && mp >= -3 && globalModifiers.contains(OrleansPrayer)) {
        newGlobalModifiers -= OrleansPrayer
        mp + 3
      } else {
        mp
      }
    }

    GameState(
      turnNumber + 1,
      newPressTurns,
      demonMp + (demonId -> newDemonMp),
      newGlobalModifiers,
      newDemonModifiers,
    )
  }

  def regenMp(demonId: DemonId, divine: Boolean) = {
    // TODO: Infinite Chakra
    val previousMp = demonMp(demonId)
    var regen = if (divine) 4 else 3

    // TODO: Mana Bonus/Mana Gain
    val newMp = Math.min(10, previousMp + regen)
    GameState(turnNumber, pressTurns, demonMp + (demonId -> newMp), globalModifiers, demonModifiers)
  }
}

object GameState {
  def initial(demonIds: Seq[DemonId]): GameState = {
    GameState(
      0,
      demonIds.length,
      (demonIds map { (_ -> Dx2Plan.initialMp) }).toMap,
      Set[GlobalModifier](),
      (demonIds map { (_ -> Set[DemonModifier]()) }).toMap,
    )
  }
}
