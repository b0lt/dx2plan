package dx2plan

import java.util.Base64

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.JSConverters._

import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLInputElement, HTMLSelectElement}

import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import rx._

import upickle.default._

import dx2db._

// Identifier of demon based on input box layout (i.e. 0 is left-most)
case class ConfigurationId(id: Int)
object ConfigurationId {
  implicit val rw: ReadWriter[ConfigurationId] = macroRW
}

case class DemonConfiguration(demon: Var[Option[Demon]] = Var[Option[Demon]](None),
                              archetype: Var[Archetype] = Var[Archetype](Archetype.Clear),
                              divine: Var[Boolean] = Var[Boolean](false),
                              lead: Var[Boolean] = Var[Boolean](false),
                              transferSkill0: Var[Option[Skill]] = Var(None),
                              transferSkill1: Var[Option[Skill]] = Var(None),
                              actions: List[Var[Move]] = List.tabulate(Dx2Plan.maxTurnsPerDemon)(_ => Var[Move](Pass)))

case class SerializedDemonConfiguration(demon: Option[DemonId], archetype: Archetype, divine: Boolean, lead: Boolean,
                                        transferSkill0: Option[SkillId], transferSkill1: Option[SkillId],
                                        actions: List[String]) {
  def applyToConfig(config: DemonConfiguration) {
    config.demon() = demon flatMap { Dx2Plan.db.demons.get }
    config.archetype() = archetype
    config.divine() = divine
    config.lead() = lead
    config.transferSkill0() = transferSkill0.flatMap(Dx2Plan.db.skills.get)
    config.transferSkill1() = transferSkill1.flatMap(Dx2Plan.db.skills.get)
    actions.zipWithIndex.foreach { case (action, index) => {
      config.actions(index)() = Move.deserialize(action)
    }}
  }
}

object SerializedDemonConfiguration {
  implicit val rw: ReadWriter[SerializedDemonConfiguration] = macroRW

  def fromConfig(config: DemonConfiguration)(implicit ctx: Ctx.Owner, data: Ctx.Data) = {
    val demon = config.demon().map(_.id)
    val archetype = config.archetype()
    val divine = config.divine()
    val lead = config.lead()
    val transferSkill0 = config.transferSkill0().map(_.id)
    val transferSkill1 = config.transferSkill1().map(_.id)
    val actions = config.actions.map { action => action().serialize() }
    SerializedDemonConfiguration(demon, archetype, divine, lead, transferSkill0, transferSkill1, actions)
  }
}

object DemonConfiguration {
  def serialize(config: DemonConfiguration)(implicit ctx: Ctx.Owner, data: Ctx.Data): SerializedDemonConfiguration = {
    SerializedDemonConfiguration.fromConfig(config)
  }

  def serialize(config: Map[ConfigurationId, DemonConfiguration])(implicit ctx: Ctx.Owner, data: Ctx.Data): String = {
    val bytes = writeBinary(config.map { case (key, value) => (key -> serialize(value)) }.toMap)
    var compressedBytes: Array[Byte] = LZMA.compress(bytes.toJSArray, 1).toArray
    Base64.getUrlEncoder().encodeToString(compressedBytes)
  }

  def deserialize(data: String) = {
    try {
      val bytes = Base64.getUrlDecoder().decode(data)
      var decompressedBytes: Array[Byte] = LZMA.decompress(bytes.toJSArray).toArray
      val serializedConfigs = readBinary[Map[ConfigurationId, SerializedDemonConfiguration]](decompressedBytes)
      Some(serializedConfigs)
    } catch {
      case ex: Throwable => {
        println(s"Failed to deserialize data: '$data'")
        ex.printStackTrace()
        None
      }
    }
  }
}

object Dx2Plan extends JSApp {
  import Framework._
  implicit val ctx: Ctx.Owner = Ctx.Owner.safe()

  val db: Database = read[Database](Dx2DbBlob.blob)

  val maxDemonCount = 4
  val maxTurnsPerDemon = 4
  val initialMp = 2

  // Underlying reactive variables for demon configuration.
  val rxConfigurations: Map[ConfigurationId, DemonConfiguration] = (0 until maxDemonCount).map { id =>
    ConfigurationId(id) -> DemonConfiguration()
  }.toMap

  // Actually selected demons.
  val rxDemons = Rx {
    rxConfigurations.filterNot { case (_, value) => value.demon().isEmpty }
  }

  // List of ConfigurationIds ordered by turn order.
  val rxOrdering = Rx {
    val demonInfo = rxDemons().map { case (id, configuration) => {
      val agi = configuration.demon().get.stats.agility
      val lead = configuration.lead()
      (id, agi, lead)
    }}
    val agiSorted = demonInfo.toSeq.sortWith((left, right) => left._2 > right._2)

    // TODO: Handle lead brands.
    agiSorted.map { case (index, agi, lead) => index }
  }

  def hasSkill(demonId: ConfigurationId, skillName: String)(implicit ctx: Ctx.Owner, data: Ctx.Data): Boolean = {
    val rxSkills = rxDemonSkills(demonId)
    rxSkills().find((skill: Spell) => {
      skill.skill.name == skillName
    }) match {
      case Some(skill) => true
      case None => false
    }
  }

  val rxGameStates: Seq[Rx[GameState]] = {
    val lb = ListBuffer[Rx[GameState]]()
    var lastState: Option[Rx[GameState]] = None
    (0 until maxDemonCount * maxTurnsPerDemon).foreach { n =>
      lastState match {
        case None => {
          val initialState = Rx {
            val demons = rxDemons()
            val ordering = rxOrdering()
            val demonId = ordering(0)

            var mpRegenBonus = 0
            if (demons(demonId).divine()) mpRegenBonus += 1
            if (hasSkill(demonId, "Infinite Chakra")) mpRegenBonus += 1

            var maxMpBonus = 0
            if (hasSkill(demonId, "Mana Bonus")) maxMpBonus += 1
            if (hasSkill(demonId, "Mana Gain")) maxMpBonus += 2
            if (hasSkill(demonId, "Mana Surge")) maxMpBonus += 3

            GameState.initial(demons.keys.toSeq).regenMp(demonId, mpRegenBonus, maxMpBonus)
          }
          lb += initialState
          lastState = Some(initialState)
        }

        case Some(previousState) => {
          require(n > 0)
          val nextState = Rx {
            val prev = previousState()

            val demons = rxDemons()
            val ordering = rxOrdering()
            require(demons.size == ordering.size)

            val demonId = ordering(n % demons.size)

            var mpRegenBonus = 0
            if (demons(demonId).divine()) mpRegenBonus += 1
            if (hasSkill(demonId, "Infinite Chakra")) mpRegenBonus += 1

            var maxMpBonus = 0
            if (hasSkill(demonId, "Mana Bonus")) maxMpBonus += 1
            if (hasSkill(demonId, "Mana Gain")) maxMpBonus += 1
            if (hasSkill(demonId, "Mana Surge")) maxMpBonus += 1

            val previousConfigurationId = ordering((n - 1) % demons.size)
            val previousDemonTurn = (n - 1) / demons.size
            val rxPreviousDemonMove = demons(previousConfigurationId).actions(previousDemonTurn)

            // TODO: Lunar Blessing (should it happen on evaluation, or on spell creation?)
            prev.evaluate(previousConfigurationId, rxPreviousDemonMove()).regenMp(demonId, mpRegenBonus, maxMpBonus)
          }
          lb += nextState
          lastState = Some(nextState)
        }
      }
    }
    lb.toSeq
  }

  val rxDemonSkills = ((0 until maxDemonCount) map { i: Int => {
    val demonId = ConfigurationId(i)
    (demonId -> Rx {
      val configuration = rxConfigurations(demonId)
      val skills = ListBuffer[Spell]()
      configuration.demon() foreach { demon =>
        demon.baseSkills.foreach { skill =>
          skills += Spell(Dx2Plan.db.skills(skill), false)
        }
        demon.awakenSkills.get(configuration.archetype()) match {
          case Some(skill) => {
            skills += Spell(Dx2Plan.db.skills(skill), true)
          }
          case None => {}
        }
        Seq(configuration.transferSkill0, configuration.transferSkill1) foreach {
          rxTransferSkill => {
            val transferSkill = rxTransferSkill()
            transferSkill foreach { skill =>
              skills += Spell(skill, false)
            }
          }
        }
      }

      skills.toList
    })
  }}).toMap

  def generateDemonConfiguration(configurationId: ConfigurationId, serializedConfig: Option[SerializedDemonConfiguration]) = {
    val idx = configurationId.id
    val demonNameId = s"demon${idx}";
    val demonArchetypeId = s"demon${idx}Archetype";
    val demonDivineId = s"demon${idx}Divine";
    val demonLeadId = s"demon${idx}Lead";
    val configuration = rxConfigurations(configurationId)
    val (selectedArchetype, divine, lead): Tuple3[Archetype, Boolean, Boolean] = serializedConfig.map {
      config => (config.archetype, config.divine, config.lead)
    }.getOrElse((Archetype.Clear, false, false))

    val transferSkills: Seq[Option[SkillId]] = serializedConfig.map {
      config => Seq(config.transferSkill0, config.transferSkill1)
    }.getOrElse(Seq(None, None))

    (
      input(
        `class` := "form-control",
        id := demonNameId,
        autofocus := idx == 0,
        tabindex := idx * 10 + 1,
        autocomplete := "false",
        value := Rx { configuration.demon().map(_.name).getOrElse("") },
        oninput := ({(elem: HTMLInputElement) => {
          configuration.demon() = Dx2Plan.db.demons.get(elem.value)
        }}: js.ThisFunction)
      ),
      select(
        `class` := "form-control",
        id := demonArchetypeId,
        tabindex := idx * 10 + 2,
        oninput := ({(elem: HTMLSelectElement) => {
          configuration.archetype() = elem.value match {
            case "clear" => Archetype.Clear
            case "red" => Archetype.Red
            case "yellow" => Archetype.Yellow
            case "purple" => Archetype.Purple
            case "teal" => Archetype.Teal
          }
        }}: js.ThisFunction),
        Archetype.all().map(
          archetype => {
            option(
              value := archetype.stringValue,
              if (selectedArchetype == archetype) selected := true,
              archetype.toString()
            )
          }
        ).toSeq
      ),
      div(
        div(
          `class` := "form-check form-check-inline",
          input(
            `class` := "form-check-input",
            id := demonDivineId,
            tabindex := idx * 10 + 3,
            `type` := "checkbox",
            if (divine) checked := true,
            onchange := ({(elem: HTMLInputElement) => {
              configuration.divine() = elem.checked
            }}: js.ThisFunction)
          ),
          label(
            `class` := "form-check-label",
            `for` := demonDivineId,
            "Divine"
          ),
        ),
        // TODO: Actually implement sorting that takes lead brands into account.*/
        div(
          `class` := "form-check form-check-inline",
          input(
            `class` := "form-check-input",
            id := demonLeadId,
            tabindex := idx * 10 + 4,
            `type` := "checkbox",
            disabled := true,
            if (lead) checked := true,
            onchange := ({(elem: HTMLInputElement) => {
              configuration.lead() = elem.checked
            }}: js.ThisFunction)
          ),
          label(
            `class` := "form-check-label",
            `for` := demonLeadId,
            "Lead"
          ),
        ),
      ),
      {
        val baseSkills: Seq[Rx.Dynamic[Option[Skill]]] = (0 until 3) map {
          i => Rx {
            val demonOpt = configuration.demon()
            demonOpt flatMap { demon =>
              if (demon.baseSkills.length > i) {
                Some(Dx2Plan.db.skills(demon.baseSkills(i)))
              } else {
                None
              }
            }
          }
        }

        val awakenSkill: Rx.Dynamic[Option[Skill]] = Rx {
          val demonOpt = configuration.demon()
          val archetype = configuration.archetype()
          demonOpt flatMap { _.awakenSkills.get(archetype).map(Dx2Plan.db.skills.apply) }
        }

        val lockedSkills = baseSkills.map((_, false)) ++ Seq((awakenSkill, true))
        val lockedSkillElements = lockedSkills map {
          case (rxSkill, awakened) => Rx {
            rxSkill() match {
              case Some(skill) => {
                div(
                  `class` := "row",
                  div(
                    `class` := "input-group",
                    style := "padding-left: 0.75rem; padding-right: 0.75rem",
                    input(
                      `type` := "text",
                      `class` := "form-control",
                      style := "background-color: #fff; text-overflow: ellipsis",
                      placeholder := skill.name,
                      disabled := true,
                    ),
                    div(
                      `class` := "input-group-append",
                      span(
                        `class` := "input-group-text",
                        skill.cost match {
                          case Some(cost) => {
                            val adjustedCost = if (awakened) {
                              cost - 1
                            } else {
                              cost
                            }
                            s"$adjustedCost MP"
                          }

                          case None => "0 MP"
                        }
                      )
                    )
                  )
                )
              }

              case None => {
                div(
                  `class` := "row",
                  div(
                    `class` := "input-group",
                    style := "padding-left: 0.75rem; padding-right: 0.75rem",
                    input(
                      `type` := "text",
                      `class` := "form-control",
                      style := "background-color: #fff; border: 0px",
                      placeholder := "",
                      disabled := true,
                    ),
                  )
                )
              }
            }
          }
        }

        val transferSkillElements = (0 until 2).map {
          i => Rx {
            val rxTransferSkill = if (i == 0) {
              configuration.transferSkill0
            } else {
              configuration.transferSkill1
            }

            div(
              `class` := "row",
              div(
                `class` := "input-group",
                style := "padding-left: 0.75rem; padding-right: 0.75rem",
                input(
                  `type` := "text",
                  `class` := "form-control",
                  value := transferSkills(i).flatMap(Dx2Plan.db.skills.get).map(_.name).getOrElse(""),
                  oninput := ({(elem: HTMLInputElement) => {
                    rxTransferSkill() = Dx2Plan.db.skills.get(elem.value)
                  }}: js.ThisFunction)
                ),
                div(
                  `class` := "input-group-append",
                  span(
                    `class` := "input-group-text",
                    Rx {

                      rxTransferSkill() match {
                        case Some(skill) => {
                          s"${skill.cost.getOrElse(0)} MP"
                        }

                        case None => "0 MP"
                      }
                    }
                  )
                )
              )
            )
          }
        }

        lockedSkillElements ++ transferSkillElements
      }
    )
  }

  val moveConfigurationTable = Rx {
    val demons = rxDemons()
    val ordering = rxOrdering()
    require(demons.size == ordering.size)
    val turnCount = demons.size * maxTurnsPerDemon
    val colNinth = "flex-basis: 11.111111%; max-width: 11.111111%; "
    val rows = (0 until turnCount) map { turn => {
      val round = turn / ordering.size
      val demonOrder = turn % ordering.size
      val configurationId = ordering(demonOrder)

      val configuration = demons(configurationId)
      require(!configuration.demon().isEmpty)

      val demonName = configuration.demon().get.name

      val rxGameState = rxGameStates(turn)
      val mp = rxGameState().demonMp(configurationId)
      val orleans = rxGameState().globalModifiers.contains(OrleanPrayer)
      val pressTurns = rxGameState().pressTurns
      if (pressTurns > 0) {
        val rxSelectedAction = configuration.actions(round)
        val rxSkills = rxDemonSkills(configurationId)
        val skills = rxSkills() filter { skill => skill.mpCost > 0 }
        val moves: Seq[Move] = Seq(Pass, Attack) ++ skills
        val selectedAction = rxSelectedAction()
        val buttons = moves.zipWithIndex.map { case (move, index) => {
          val buttonId = s"turn${turn}_${index}"
          val enoughMp = mp >= move.mpCost
          val selected = selectedAction == move

          val buttonColor = (move.name) match {
            case "Pass" => "secondary"
            case "Attack" => "info"
            case "Tag" => "warning"
            case _ => {
              if (mp >= move.mpCost) {
                "primary"
              } else if (mp + 3 >= move.mpCost && orleans) {
                "success"
              } else {
                "danger"
              }
            }
          }

          val buttonSelection = if (selected) "" else "-outline"
          val buttonClass = s"btn$buttonSelection-$buttonColor"

          div(
            `class` := "col-2 align-self-center",
            style := colNinth + "height: 80%",
            button(
              name := s"turn${turn}",
              `class` := s"btn $buttonClass h-100 w-100",
              style := "padding-left: 0px; padding-right: 0px; font-size: 0.75rem; font-weight: bold",
              id := buttonId,
              onclick := ({(elem: HTMLInputElement) => {
                val rxAction = configuration.actions(round)
                rxAction() = move
              }}: js.ThisFunction),
              move.name,
              if (move.mpCost != 0) { br },
              if (move.mpCost != 0) { s"${move.mpCost} MP" },
            ),
          )
        }}.toList

        val row = ListBuffer[Frag]()
        row += div(
          `class` := "col-2 h-100",
          style := colNinth + "display: table",
          div(
            style := "height: 100%; display: table-cell; vertical-align: middle",
            div(
              `class` := "row",
              style := "margin-left:0px",
              strong(demonName)
            ),
            div(
              `class` := "row",
              style := "margin-left:0px",
              if (orleans) {
                s"$mp [${mp + 3}] MP"
              } else {
                s"$mp MP"
              }
            ),
            div(
              `class` := "row",
              style := "margin-left:0px",
              s"$pressTurns press turns"
            ),
            div(`class` := "row",
              style := "margin-left:0px",
              em(rxGameState().demonModifiers(configurationId).mkString(", "))
            ),
          )
        )
        row ++= buttons
        row += div(`class` := "w-100")
        Some(row.toList)
      } else {
        None
      }
    }}

    div(
      `class` := "row",
      style := "height: 100px",
      rows.filterNot(_.isEmpty).map(_.get)
    )
  }

  val permalink = Rx {
    div(
      a(
        href := s"#${DemonConfiguration.serialize(rxConfigurations)}",
        s"Permalink"
      ),
    )
  }

  def main(): Unit = {
    dom.document.body.innerHTML = ""

    val serializedConfigs: Option[Map[ConfigurationId, SerializedDemonConfiguration]] = dom.document.location.hash match {
      case "#example" => {
        val ishtar = SerializedDemonConfiguration(
          Some(Dx2Plan.db.demons("Ishtar").id),
          Archetype.Yellow,
          divine = true, lead = false,
          None, None,
          List("Concentrate (awakened)", "Attack", "Mesopotamian Star", "Attack")
        )
        val fenrir = SerializedDemonConfiguration(
          Some(Dx2Plan.db.demons("Fenrir").id),
          Archetype.Purple,
          divine = false, lead = false,
          Some(Dx2Plan.db.skills("Rakunda").id), Some(Dx2Plan.db.skills("Makarakarn").id),
          List("Pass", "Pass", "Pass", "Pass"),
        )
        val pyro = SerializedDemonConfiguration(
          Some(Dx2Plan.db.demons("Pyro Jack").id),
          Archetype.Yellow,
          divine = false, lead = false,
          Some(Dx2Plan.db.skills("Megido").id), None,
          List("Tag (awakened)", "Tag (awakened)", "Tag (awakened)", "Tag (awakened)"),
        )
        val jack = SerializedDemonConfiguration(
          Some(Dx2Plan.db.demons("Jack Frost").id),
          Archetype.Yellow,
          divine = false, lead = false,
          None, None,
          List("Tag (awakened)", "Tag (awakened)", "Tag (awakened)", "Tag (awakened)"),
        )

        Some(
          Map(
            (ConfigurationId(0) -> ishtar),
            (ConfigurationId(1) -> fenrir),
            (ConfigurationId(2) -> pyro),
            (ConfigurationId(3) -> jack)
          )
        )
      }

      case "" => None
      case "#" => None

      case _ => {
        DemonConfiguration.deserialize(dom.document.location.hash.substring(1))
      }
    }

    serializedConfigs foreach { configs =>
      configs map {
        case (id, serializedConfig) => {
          serializedConfig.applyToConfig(rxConfigurations(id))
        }
      }
    }

    val demonConfigurationElements = (0 until maxDemonCount) map {
      i => generateDemonConfiguration(ConfigurationId(i), serializedConfigs.map(config => config(ConfigurationId(i))))
    }

    val container = dom.document.body.appendChild(
      div(
        `class` := "container-fluid",
        style := "width: 1100px",
      ).render
    )

    val col = `class` := "col"
    val colSmall = `class` := "col-2"
    val rowBreak = div(`class` := "w-100")
    container.appendChild(
      div(
        div(
          `class` := "row",
          div(
            `class` := "col",
            h1("dx2plan"),
          ),
          div(
            `class` := "text-right align-self-center",
            permalink
          )
        ),

        div(
          `class` := "row",

          div(colSmall, strong("Demon")),
          demonConfigurationElements.map(elements => div(col, elements._1)),

          rowBreak,
          div(colSmall, strong("Archetype")),
          demonConfigurationElements.map(elements => div(col, elements._2)),

          rowBreak,
          div(colSmall, strong("Brands")),
          demonConfigurationElements.map(elements => div(col, elements._3)),

          rowBreak,
          div(colSmall, strong("Skills")),
          demonConfigurationElements.map(elements => div(col, elements._4)),
        )
      ).render
    )

    container.appendChild(
      div(
        moveConfigurationTable
      ).render
    )
  }
}