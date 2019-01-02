package dx2plan

import dx2db._

sealed trait Move {
  def name: String
  def element: Option[Element]
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
  def element = None
  def mpCost = 0
  def pressTurnCost = 0.5
}

case object Attack extends Move {
  def name = "Attack"
  def element = Some(Element.Phys)
  def mpCost = 0
  def pressTurnCost = 1
}

case class Spell(skill: Skill, awakened: Boolean) extends Move {
  def name = skill.name
  def element = skill match {
    case active: Skill.Active => active.element
    case passive: Skill.Passive => None
  }

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
case object OrleanPrayer extends GlobalModifier

sealed trait DemonModifier {
  def consumedBy(element: Option[Element]): Boolean
}

case object Rebellion extends DemonModifier {
  def consumedBy(element: Option[Element]) = element.map(_.isPhysical).getOrElse(false)
}

case object Charge extends DemonModifier {
  def consumedBy(element: Option[Element]) = element.map(_.isPhysical).getOrElse(false)
}

case object Concentrate extends DemonModifier {
  def consumedBy(element: Option[Element]) = element.map(_.isMagical).getOrElse(false)
}

object Move {
  def deserialize(string: String): Move = {
    val awakened = raw"(.+) \(awakened\)".r
    string match {
      case "Pass" => Pass
      case "Attack" => Attack
      case awakened(name) => Dx2Plan.db.skills.get(name) match {
        case Some(skill) => Spell(skill.asActive, true)
        case None => Pass
      }
      case unawakened => Dx2Plan.db.skills.get(unawakened) match {
        case Some(skill) => Spell(skill.asActive, false)
        case None => Pass
      }
    }
  }
}

case class GameState(turnNumber: Int, pressTurns: Double, demonMp: Map[ConfigurationId, Int],
                     globalModifiers: Set[GlobalModifier], demonModifiers: Map[ConfigurationId, Set[DemonModifier]]) {
  def evaluate(configurationId: ConfigurationId, move: Move) = {
    var newDemonModifiers = demonModifiers
    var newGlobalModifiers = globalModifiers
    var pressTurnCost = move.pressTurnCost

    if (move.element == Some(Element.Phys) && demonModifiers(configurationId).contains(Rebellion)) {
      pressTurnCost = 0.5
    }

    val newDemonMp = {
      val mp = demonMp(configurationId) - move.mpCost
      if (mp < 0 && mp >= -3 && globalModifiers.contains(OrleanPrayer)) {
        newGlobalModifiers -= OrleanPrayer
        mp + 3
      } else {
        mp
      }
    }

    val newPressTurns = {
      if (pressTurnCost == 0) {
        pressTurns
      } else if (pressTurns % 1 == 0) {
        pressTurns - pressTurnCost
      } else {
        pressTurns - 0.5
      }
    }

    move.name match {
      case "Charge" => {
        newDemonModifiers += (configurationId -> (demonModifiers(configurationId) + Charge - Concentrate))
      }

      case "Concentrate" => {
        newDemonModifiers += (configurationId -> (demonModifiers(configurationId) + Concentrate - Charge))
      }

      case "Rebellion" => {
        newDemonModifiers += (configurationId -> (demonModifiers(configurationId) + Rebellion))
      }

      case "Mega Boost" => {
        newDemonModifiers += (configurationId -> (demonModifiers(configurationId) + Charge + Rebellion))
      }

      case "Red Zone" => {
        for (i <- 0 until 4) {
          val id = ConfigurationId(i)
          newDemonModifiers += (id -> (demonModifiers(id) + Rebellion))
        }
      }

      case "Orlean Prayer" => {
        newGlobalModifiers += OrleanPrayer
      }

      case "Silent Prayer" => {
        newDemonModifiers = demonModifiers.map { case (id, _) => id -> Set[DemonModifier]() }.toMap
      }

      case _ => {}
    }

    newDemonModifiers =
      newDemonModifiers +
      (configurationId -> newDemonModifiers(configurationId).filterNot(_.consumedBy(move.element)))

    GameState(
      turnNumber + 1,
      newPressTurns,
      demonMp + (configurationId -> newDemonMp),
      newGlobalModifiers,
      newDemonModifiers,
    )
  }

  def regenMp(configurationId: ConfigurationId, mpRegenBonus: Int, maxMpBonus: Int) = {
    val previousMp = demonMp(configurationId)
    var regen = 3 + mpRegenBonus
    val newMp = Math.min(10 + maxMpBonus, previousMp + regen)
    GameState(turnNumber, pressTurns, demonMp + (configurationId -> newMp), globalModifiers, demonModifiers)
  }
}

object GameState {
  def initial(configurationIds: Seq[ConfigurationId], initialMp: Int): GameState = {
    GameState(
      0,
      configurationIds.length,
      (configurationIds map { (_ -> initialMp) }).toMap,
      Set[GlobalModifier](),
      (configurationIds map { (_ -> Set[DemonModifier]()) }).toMap,
    )
  }
}
