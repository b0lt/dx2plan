package dx2plan

import dx2db._

import upickle.default.{ReadWriter => RW, macroRW, readwriter}

sealed trait UsageType {
  def next(skill: Skill.Active): UsageType
}

object UsageType {
  implicit val rw: RW[UsageType] = RW.merge(macroRW[Normal.type], macroRW[Critical.type], macroRW[Null.type])

  case object Normal extends UsageType {
    def next(skill: Skill.Active) = {
      skill.element match {
        case Some(Element.Almighty) => Null
        case Some(element) => Critical
        case None => Normal
      }
    }
  }

  case object Critical extends UsageType {
    def next(skill: Skill.Active) = Null
  }

  case object Null extends UsageType {
    def next(skill: Skill.Active) = Normal
  }
}

sealed trait Move {
  def name: String
  def postscript: Option[String] = None

  def element: Option[Element]
  def mpCost: Int = 0
  def mpRegen: Int = 0
  def pressTurnCost: Double

  def nextType: Move
  def usageType: UsageType
  def isSameAction(move: Move): Boolean
}
object Move {
  implicit val rw: RW[Move] = macroRW
}

case class Pass(usageType: UsageType = UsageType.Normal) extends Move {
  def name = usageType match {
    case UsageType.Normal => "Pass"
    case UsageType.Critical => "Lucky Skip"
    case _ => ???
  }

  def element = None
  def pressTurnCost = usageType match {
    case UsageType.Normal => 0.5
    case UsageType.Critical => 0
    case _ => ???
  }

  def nextType = usageType match {
    case UsageType.Normal => Pass(UsageType.Critical)
    case UsageType.Critical => Pass(UsageType.Normal)
    case _ => ???
  }

  def isSameAction(action: Move) = action match {
    case Pass(_) => true
    case _ => false
  }
}
object Pass {
  implicit val rw: RW[Pass] = macroRW
}

case class Attack(usageType: UsageType = UsageType.Normal) extends Move {
  def name = usageType match {
    case UsageType.Normal => "Attack"
    case UsageType.Critical => "Critical"
    case UsageType.Null => "Null"
  }

  def element = Some(Element.Phys)
  def pressTurnCost = usageType match {
    case UsageType.Normal => 1
    case UsageType.Critical => 0.5
    case UsageType.Null => 2
  }

  def nextType = usageType match {
    case UsageType.Normal => Attack(UsageType.Critical)
    case UsageType.Critical => Attack(UsageType.Null)
    case UsageType.Null => Attack(UsageType.Normal)
  }

  def isSameAction(action: Move) = action match {
    case Attack(_) => true
    case _ => false
  }
}
object Attack {
  implicit val rw: RW[Attack] = macroRW
}

case class SkillInstance(skill: Skill, awakened: Boolean, level: Int) {
  def cost: Int = {
    if (awakened) {
      skill.cost(level).map(_ - 1).getOrElse(0)
    } else {
      skill.cost(level).getOrElse(0)
    }
  }

  def mpRegen: Int = skill.mpRegen(level).getOrElse(0)
}
object SkillInstance {
  implicit val rw: RW[SkillInstance] = readwriter[(SkillId, Boolean, Int)].bimap[SkillInstance](
    skillInstance => (skillInstance.skill.id, skillInstance.awakened, skillInstance.level),
    { case (id: SkillId, awakened: Boolean, level: Int) => SkillInstance(Dx2Plan.db.skills(id), awakened, level) }
  )

  def apply(skill: Skill, awakened: Boolean): SkillInstance = SkillInstance(skill, awakened, skill.maxLevel)
}

case class SkillUsage(skillInstance: SkillInstance, usageType: UsageType = UsageType.Normal) extends Move {
  def name = skillInstance.skill.name
  override def postscript = usageType match {
    case UsageType.Normal => None
    case UsageType.Critical => {
      if (element.get.isMagical) {
        Some("Weakness")
      } else {
        Some("Critical")
      }
    }

    case UsageType.Null => Some("Null")
  }

  def element = skillInstance.skill match {
    case active: Skill.Active => active.element
    case passive: Skill.Passive => None
  }

  override def mpCost = skillInstance.cost
  override def mpRegen = skillInstance.mpRegen

  def pressTurnCost = {
    usageType match {
      case UsageType.Normal => name match {
        case "Tag" => 0
        case _ => 1
      }

      case UsageType.Critical => 0.5
      case UsageType.Null => 2
    }
  }

  def nextType = SkillUsage(skillInstance, usageType.next(skillInstance.skill.asActive))
  def isSameAction(action: Move) = action match {
    case SkillUsage(otherSkillInstance, _) => skillInstance == otherSkillInstance
    case _ => false
  }
}

object SkillUsage {
  implicit val rw: RW[SkillUsage] = macroRW
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

case class GameState(turnNumber: Int, pressTurns: Double, demonMp: Map[ConfigurationId, Int],
                     globalModifiers: Set[GlobalModifier], demonModifiers: Map[ConfigurationId, Set[DemonModifier]]) {
  def evaluate(configurationId: ConfigurationId, move: Move) = {
    var newDemonModifiers = demonModifiers
    var newGlobalModifiers = globalModifiers
    var pressTurnCost = move.pressTurnCost

    if (move.element == Some(Element.Phys) && move.usageType != UsageType.Null) {
      if (demonModifiers(configurationId).contains(Rebellion)) {
        pressTurnCost = 0.5
      }
    }

    val newDemonMp = {
      var mp = demonMp(configurationId) - move.mpCost
      if (mp < 0 && mp >= -3 && globalModifiers.contains(OrleanPrayer)) {
        newGlobalModifiers -= OrleanPrayer
        mp += 3
      }

      mp + move.mpRegen
    }

    val newPressTurns = {
      require(pressTurnCost == 0 || pressTurnCost == 0.5 || pressTurnCost == 1 || pressTurnCost == 2)

      var x = pressTurns
      // Consume the half-turn first.
      if (x % 1 != 0 && pressTurnCost != 0) {
        x -= 0.5
        pressTurnCost = Math.max(0, pressTurnCost - 1)
      }

      // Consume whatever's left.
      x - pressTurnCost
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
