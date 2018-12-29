package dx2plan

sealed trait Move {
  def name: String
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
  def mpCost = 0
  def pressTurnCost = 0.5
}

case object Attack extends Move {
  def name = "Attack"
  def mpCost = 0
  def pressTurnCost = 1
}

case class Spell(skill: Skill, awakened: Boolean) extends Move {
  def name = skill.name
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

case class GameState(turnNumber: Int, pressTurns: Double, demonMp: Map[DemonId, Int]) {
  def evaluate(demonId: DemonId, move: Move) = {
    val newPressTurns = {
      if (move.pressTurnCost == 0) {
        pressTurns
      } else if (pressTurns % 1 == 0) {
        pressTurns - move.pressTurnCost
      } else {
        pressTurns - 0.5
      }
    }

    val newMp = demonMp + (demonId -> (demonMp(demonId) - move.mpCost))
    GameState(turnNumber + 1, newPressTurns, newMp)
  }

  def regenMp(demonId: DemonId, divine: Boolean) = {
    // TODO: Infinite Chakra
    val previousMp = demonMp(demonId)
    var regen = if (divine) 4 else 3

    // TODO: Mana Bonus/Mana Gain
    val newMp = Math.min(10, previousMp + regen)
    GameState(turnNumber, pressTurns, demonMp + (demonId -> newMp))
  }
}
