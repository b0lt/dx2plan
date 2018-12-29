package dx2plan

sealed trait Move {
  def name: String
  def mpCost: Int
  def pressTurnCost: Double

  final def serialize() = {
    this match {
      case Pass => "Pass"
      case Attack => "Attack"
      case Spell(name, cost) => s"$name ($cost MP)"
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

case class Spell(_name: String, cost: Int) extends Move {
  def name = _name
  def mpCost = cost
  def pressTurnCost = {
    name match {
      case "Tag" => 0
      case _ => 1
    }
  }
}

object Move {
  def deserialize(string: String): Move = {
    val pattern = raw"(.+) \(([0-9]+) MP\)".r
    string match {
      case "Pass" => Pass
      case "Attack" => Attack
      case pattern(name, cost) => Spell(name, cost.toInt)
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
