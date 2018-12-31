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
import upickle.default.{ReadWriter => RW, macroRW}

// Identifier of demon based on input box layout (i.e. 0 is left-most)
case class DemonId(id: Int)
object DemonId {
  implicit val rw: RW[DemonId] = macroRW
}

case class DemonConfiguration(demon: Var[Option[Demon]] = Var[Option[Demon]](None),
                              color: Var[Color] = Var[Color](Color.Clear),
                              divine: Var[Boolean] = Var[Boolean](false),
                              lead: Var[Boolean] = Var[Boolean](false),
                              transferSkill0: Var[Option[Skill]] = Var(None),
                              transferSkill1: Var[Option[Skill]] = Var(None),
                              actions: List[Var[Move]] = List.tabulate(Dx2Plan.maxTurnsPerDemon)(_ => Var[Move](Pass)))

case class SerializedDemonConfiguration(demon: String, color: String, divine: Boolean, lead: Boolean,
                                        transferSkill0: String, transferSkill1: String, actions: List[String]) {
  def applyToConfig(config: DemonConfiguration) {
    config.demon() = Demon.find(demon)
    config.color() = Color.deserialize(color)
    config.divine() = divine
    config.lead() = lead
    config.transferSkill0() = Skill.find(transferSkill0)
    config.transferSkill1() = Skill.find(transferSkill1)
    actions.zipWithIndex.foreach { case (action, index) => {
      config.actions(index)() = Move.deserialize(action)
    }}
  }
}

object SerializedDemonConfiguration {
  implicit val rw: RW[SerializedDemonConfiguration] = macroRW

  def fromConfig(config: DemonConfiguration)(implicit ctx: Ctx.Owner, data: Ctx.Data) = {
    val demon = config.demon().map(_.name).getOrElse("")
    val color = config.color().serialize()
    val divine = config.divine()
    val lead = config.lead()
    val transferSkill0 = config.transferSkill0().map(_.name).getOrElse("")
    val transferSkill1 = config.transferSkill1().map(_.name).getOrElse("")
    val actions = config.actions.map { action => action().serialize() }
    SerializedDemonConfiguration(demon, color, divine, lead, transferSkill0, transferSkill1, actions)
  }
}

object DemonConfiguration {
  def serialize(config: DemonConfiguration)(implicit ctx: Ctx.Owner, data: Ctx.Data): SerializedDemonConfiguration = {
    SerializedDemonConfiguration.fromConfig(config)
  }

  def serialize(config: Map[DemonId, DemonConfiguration])(implicit ctx: Ctx.Owner, data: Ctx.Data): String = {
    val bytes = writeBinary(config.map { case (key, value) => (key -> serialize(value)) }.toMap)
    var compressedBytes: Array[Byte] = LZMA.compress(bytes.toJSArray, 1).toArray
    Base64.getUrlEncoder().encodeToString(compressedBytes)
  }

  def deserialize(data: String) = {
    try {
      val bytes = Base64.getUrlDecoder().decode(data)
      var decompressedBytes: Array[Byte] = LZMA.decompress(bytes.toJSArray).toArray
      val serializedConfigs = readBinary[Map[DemonId, SerializedDemonConfiguration]](decompressedBytes)
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

  val maxDemonCount = 4
  val maxTurnsPerDemon = 4
  val initialMp = 2

  // Underlying reactive variables for demon configuration.
  val rxConfigurations: Map[DemonId, DemonConfiguration] = (0 until maxDemonCount).map { id =>
    DemonId(id) -> DemonConfiguration()
  }.toMap

  // Actually selected demons.
  val rxDemons = Rx {
    rxConfigurations.filterNot { case (_, value) => value.demon().isEmpty }
  }

  // List of DemonIds ordered by turn order.
  val rxOrdering = Rx {
    val demonInfo = rxDemons().map { case (id, configuration) => {
      val agi = configuration.demon().get.stats(Stat.Agility)
      val lead = configuration.lead()
      (id, agi, lead)
    }}
    val agiSorted = demonInfo.toSeq.sortWith((left, right) => left._2 > right._2)

    // TODO: Handle lead brands.
    agiSorted.map { case (index, agi, lead) => index }
  }

  def hasSkill(demonId: DemonId, skillName: String)(implicit ctx: Ctx.Owner, data: Ctx.Data): Boolean = {
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

            val previousDemonId = ordering((n - 1) % demons.size)
            val previousDemonTurn = (n - 1) / demons.size
            val rxPreviousDemonMove = demons(previousDemonId).actions(previousDemonTurn)

            // TODO: Lunar Blessing (should it happen on evaluation, or on spell creation?)
            prev.evaluate(previousDemonId, rxPreviousDemonMove()).regenMp(demonId, mpRegenBonus, maxMpBonus)
          }
          lb += nextState
          lastState = Some(nextState)
        }
      }
    }
    lb.toSeq
  }

  val rxDemonSkills = ((0 until maxDemonCount) map { i: Int => {
    val demonId = DemonId(i)
    (demonId -> Rx {
      val configuration = rxConfigurations(demonId)
      val skills = ListBuffer[Spell]()
      configuration.demon() foreach { demon =>
        demon.baseSkills.foreach { skill =>
          skills += Spell(skill, false)
        }
        demon.awakenSkills(configuration.color()) match {
          case Some(skill) => {
            skills += Spell(skill, true)
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

  def generateDemonConfiguration(demonId: DemonId, serializedConfig: Option[SerializedDemonConfiguration]) = {
    val idx = demonId.id
    val demonNameId = s"demon${idx}";
    val demonArchetypeId = s"demon${idx}Archetype";
    val demonDivineId = s"demon${idx}Divine";
    val demonLeadId = s"demon${idx}Lead";
    val demon = rxConfigurations(demonId)
    val (color, divine, lead): Tuple3[Color, Boolean, Boolean] = serializedConfig.map {
      config => (Color.deserialize(config.color), config.divine, config.lead)
    }.getOrElse((Color.Clear, false, false))

    val transferSkills = serializedConfig.map {
      config => Seq(config.transferSkill0, config.transferSkill1)
    }.getOrElse(Seq("", ""))

    (
      input(
        `class` := "form-control",
        id := demonNameId,
        autofocus := idx == 0,
        tabindex := idx * 10 + 1,
        autocomplete := "false",
        value := Rx { demon.demon().map(_.name).getOrElse("") },
        oninput := ({(elem: HTMLInputElement) => {
          rxConfigurations(demonId).demon() = Demon.find(elem.value)
        }}: js.ThisFunction)
      ),
      select(
        `class` := "form-control",
        id := demonArchetypeId,
        tabindex := idx * 10 + 2,
        oninput := ({(elem: HTMLSelectElement) => {
          rxConfigurations(demonId).color() = elem.value match {
            case "clear" => Color.Clear
            case "red" => Color.Red
            case "yellow" => Color.Yellow
            case "purple" => Color.Purple
            case "teal" => Color.Teal
          }
        }}: js.ThisFunction),
        option(
          value := "clear",
          if (color == Color.Clear) selected := true,
          "Clear"),
        option(
          value := "red",
          if (color == Color.Red) selected := true,
          "Red"
        ),
        option(
          value := "yellow",
          if (color == Color.Yellow) selected := true,
          "Yellow"
        ),
        option(
          value := "purple",
          if (color == Color.Purple) selected := true,
          "Purple"
        ),
        option(
          value := "teal",
          if (color == Color.Teal) selected := true,
          "Teal"
        ),
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
              rxConfigurations(demonId).divine() = elem.checked
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
              rxConfigurations(demonId).lead() = elem.checked
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
            val demonOpt = demon.demon()
            demonOpt flatMap { demon =>
              if (demon.baseSkills.length > i) {
                Some(demon.baseSkills(i))
              } else {
                None
              }
            }
          }
        }

        val awakenSkill: Rx.Dynamic[Option[Skill]] = Rx {
          val demonOpt = demon.demon()
          val color = demon.color()
          demonOpt flatMap { _.awakenSkills(color) }
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
                    input(
                      `type` := "text",
                      `class` := "form-control",
                      style := "background-color: #fff",
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
              demon.transferSkill0
            } else {
              demon.transferSkill1
            }

            div(
              `class` := "row",
              div(
                `class` := "input-group",
                input(
                  `type` := "text",
                  `class` := "form-control",
                  value := transferSkills(i),
                  oninput := ({(elem: HTMLInputElement) => {
                    rxTransferSkill() = Skill.find(elem.value)
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
    val colEighth = "flex: 0 0 12.5%; max-width: 12.5%; "
    val rows = (0 until turnCount) map { turn => {
      val round = turn / ordering.size
      val demonOrder = turn % ordering.size
      val demonId = ordering(demonOrder)

      val demon = demons(demonId)
      require(!demon.demon().isEmpty)

      val demonName = demon.demon().get.name

      val rxGameState = rxGameStates(turn)
      val mp = rxGameState().demonMp(demonId)
      val orleans = rxGameState().globalModifiers.contains(OrleansPrayer)
      val pressTurns = rxGameState().pressTurns
      if (pressTurns > 0) {
        val rxSelectedAction = demon.actions(round)
        val rxSkills = rxDemonSkills(demonId)
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
            style := colEighth + "height: 80%",
            button(
              name := s"turn${turn}",
              `class` := s"btn $buttonClass h-100 w-100",
              style := "padding-left: 0px; padding-right: 0px; font-size: 0.75rem; font-weight: bold",
              id := buttonId,
              onclick := ({(elem: HTMLInputElement) => {
                val rxAction = rxConfigurations(demonId).actions(round)
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
          style := colEighth + "display: table",
          div(
            style := "height: 100%; display: table-cell; vertical-align: middle",
            div(
              div(`class` := "row", strong(demonName)),
              div(
                `class` := "row",
                if (orleans) {
                  s"$mp [${mp + 3}] MP"
                } else {
                  s"$mp MP"
                }
              ),
              div(`class` := "row", s"$pressTurns press turns"),
              div(`class` := "row", {
                em {
                  rxGameState().demonModifiers(demonId).mkString(", ")
                }
              }),
            )
          )
        )
        row += div(
          `class` := "col-10",
          style := "align-items: center",
          div(
            `class` := "row h-100",
            buttons
          )
        )
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

    val serializedConfigs: Option[Map[DemonId, SerializedDemonConfiguration]] = dom.document.location.hash match {
      case "#example" => {
        val ishtar = SerializedDemonConfiguration(
          "Ishtar",
          "Yellow",
          divine = true, lead = false,
          "", "",
          List("Concentrate (awakened)", "Attack", "Mesopotamian Star", "Attack")
        )
        val fenrir = SerializedDemonConfiguration(
          "Fenrir",
          "Purple",
          divine = false, lead = false,
          "Rakunda", "Makarakarn",
          List("Pass", "Pass", "Pass", "Pass")
        )
        val pyro = SerializedDemonConfiguration(
          "Pyro Jack",
          "Yellow",
          divine = false, lead = false,
          "Megido", "",
          List("Tag (awakened)", "Tag (awakened)", "Tag (awakened)", "Tag (awakened)")
        )
        val jack = SerializedDemonConfiguration(
          "Jack Frost",
          "Yellow",
          divine = false, lead = false,
          "", "",
          List("Tag (awakened)", "Tag (awakened)", "Tag (awakened)", "Tag (awakened)")
        )

        Some(
          Map(
            (DemonId(0) -> ishtar),
            (DemonId(1) -> fenrir),
            (DemonId(2) -> pyro),
            (DemonId(3) -> jack)
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
      i => generateDemonConfiguration(DemonId(i), serializedConfigs.map(config => config(DemonId(i))))
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

    container.appendChild(moveConfigurationTable.render)
  }
}