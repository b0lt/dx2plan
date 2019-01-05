package dx2plan

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.JSApp

import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLInputElement, HTMLSelectElement}

import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import rx._
import upickle.default._

import dx2db._

object Dx2Plan {
  import Framework._
  implicit val ctx: Ctx.Owner = Ctx.Owner.safe()

  val db: Database = Database.fromUrl("dx2db.json").get

  val maxDemonCount = 4
  val maxTurnsPerDemon = 4

  lazy val serializedConfiguration: Option[SerializedConfiguration] = getSerializedConfiguration()
  lazy val rxConfiguration = Configuration.fromSerializedConfiguration(serializedConfiguration)

  lazy val rxInitialMp = Rx {
    val first = rxConfiguration.first()
    if (first) {
      2
    } else {
      5
    }
  }

  lazy val rxDemons = rxConfiguration.demonConfigurations

  // Actually selected demons.
  lazy val rxSelectedDemons = Rx {
    rxDemons.filter { case (_, value) => value.demon().isDefined }
  }

  // List of ConfigurationIds ordered by turn order.
  lazy val rxOrdering = Rx {
    type OrderEntry = (ConfigurationId, Int, Boolean)

    val demonInfo: Iterable[OrderEntry] = rxSelectedDemons().map { case (id, configuration) => {
      val agi = configuration.demon().get.stats.agility
      val lead = configuration.lead()
      (id, agi, lead)
    }}
    val agiSorted = demonInfo.toSeq.sortWith((left, right) => left._2 > right._2)

    // Lead brands move demons up one slot, processed left to right.
    // (e.g. if you give [2, 3, 4] lead brands, the final ordering will be [1, 2, 3, 4])
    val leadSorted = agiSorted.foldLeft(ListBuffer[OrderEntry]())({
      (buf: ListBuffer[OrderEntry], entry: OrderEntry) => {
        val (index, agi, lead) = entry
        if (buf.isEmpty) {
          buf += entry
        } else if (!lead) {
          // No lead brand, just append ourselves.
          buf += entry
        } else if (buf.last._3) {
          // The previous demon has a lead brand, so we must be in a scenario where all preceding demons have lead.
          buf += entry
        } else {
          // We have a lead brand and the demon in front of us doesn't.
          val tail = buf.remove(buf.length - 1)
          buf += entry
          buf += tail
        }
      }
    })

    leadSorted.map { case (index, agi, lead) => index }
  }

  // TODO: Switch to using SkillId.
  def hasSkill(demonId: ConfigurationId, skillName: String)(implicit ctx: Ctx.Owner, data: Ctx.Data): Boolean = {
    val rxSkills = rxDemonSkills(demonId)
    rxSkills().find((skill: SkillUsage) => {
      skill.skillInstance.skill.name == skillName
    }) match {
      case Some(skill) => true
      case None => false
    }
  }

  lazy val rxGameStates: Seq[Rx[GameState]] = {
    val lb = ListBuffer[Rx[GameState]]()
    var lastState: Option[Rx[GameState]] = None
    (0 until maxDemonCount * maxTurnsPerDemon).foreach { n =>
      lastState match {
        case None => {
          val initialState = Rx {
            val demons = rxSelectedDemons()
            val ordering = rxOrdering()
            val demonId = ordering(0)

            var mpRegenBonus = 0
            if (demons(demonId).divine()) mpRegenBonus += 1
            if (hasSkill(demonId, "Infinite Chakra")) mpRegenBonus += 1

            var maxMpBonus = 0
            if (hasSkill(demonId, "Mana Bonus")) maxMpBonus += 1
            if (hasSkill(demonId, "Mana Gain")) maxMpBonus += 2
            if (hasSkill(demonId, "Mana Surge")) maxMpBonus += 3

            GameState.initial(demons.keys.toSeq, rxInitialMp()).regenMp(demonId, mpRegenBonus, maxMpBonus)
          }
          lb += initialState
          lastState = Some(initialState)
        }

        case Some(previousState) => {
          require(n > 0)
          val nextState = Rx {
            val prev = previousState()

            val demons = rxSelectedDemons()
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

  lazy val rxDemonBaseSkills = ((0 until maxDemonCount) map { i: Int => {
    val configurationId = ConfigurationId(i)
    (configurationId -> Rx {
      val configuration = rxDemons(configurationId)
      configuration.demon().map { demon =>
        demon.baseSkills.map { skill =>
          val skillInstance = SkillInstance(Dx2Plan.db.skills(skill), false)
          SkillUsage(skillInstance, UsageType.Normal)
        }
      }.getOrElse(Seq())
    })
  }}).toMap

  lazy val rxDemonAwakenSkill = ((0 until maxDemonCount) map { i: Int => {
    val configurationId = ConfigurationId(i)
    (configurationId -> Rx {
      val configuration = rxDemons(configurationId)
      configuration.demon().map { demon =>
        demon.awakenSkills.get(configuration.archetype()) match {
          case Some(skill) => {
            val skillInstance = SkillInstance(Dx2Plan.db.skills(skill), true)
            Some(SkillUsage(skillInstance, UsageType.Normal))
          }
          case None => None
        }
      }.getOrElse(None)
    })
  }}).toMap

  lazy val rxDemonSkills = ((0 until maxDemonCount) map { i: Int => {
    val configurationId = ConfigurationId(i)
    (configurationId -> Rx {

      val transferSkills = ListBuffer[SkillUsage]()
      val rxBaseSkills = rxDemonBaseSkills(configurationId)
      transferSkills ++= rxBaseSkills()

      val rxAwakenSkill = rxDemonAwakenSkill(configurationId)
      rxAwakenSkill().foreach { transferSkills += _ }

      val configuration = rxDemons(configurationId)
      configuration.demon() foreach { demon =>
        Seq(configuration.transferSkill0, configuration.transferSkill1) foreach {
          rxTransferSkill => {
            val transferSkill = rxTransferSkill()
            transferSkill foreach { skill =>
              val skillInstance = SkillInstance(skill, false)
              transferSkills += SkillUsage(skillInstance, UsageType.Normal)
            }
          }
        }
      }

      transferSkills
    })
  }}).toMap

  def generateDemonConfiguration(configurationId: ConfigurationId, serialized: Option[SerializedConfiguration]) = {
    val idx = configurationId.id
    val demonDivineId = s"demon${idx}Divine";
    val demonLeadId = s"demon${idx}Lead";
    val configuration = rxDemons(configurationId)

    val demonConfig = serialized.map(_.demonConfigurations(configurationId))
    val (selectedArchetype, divine, lead): Tuple3[Archetype, Boolean, Boolean] = demonConfig.map {
      config => (config.archetype, config.divine, config.lead)
    }.getOrElse((Archetype.Clear, false, false))

    val transferSkills: Seq[Option[SkillId]] = demonConfig.map {
      config => Seq(config.transferSkill0, config.transferSkill1)
    }.getOrElse(Seq(None, None))

    (
      input(
        `class` := "form-control",
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
        div(
          `class` := "form-check form-check-inline",
          input(
            `class` := "form-check-input",
            id := demonLeadId,
            tabindex := idx * 10 + 4,
            `type` := "checkbox",
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
        val rxBaseSkills = rxDemonBaseSkills(configurationId)
        val awakenSkill = rxDemonAwakenSkill(configurationId)
        val lockedSkills = (0 until 3).map {
          i => Rx {
            val baseSkills = rxBaseSkills()
            if (i < baseSkills.length) {
              Some(baseSkills(i))
            } else {
              None
            }
          }
        } ++ Seq(awakenSkill)

        val lockedSkillElements = lockedSkills.map { rxSkillUsage => Rx {
            rxSkillUsage() match {
              case Some(skillUsage) => {
                div(
                  `class` := "row",
                  div(
                    `class` := "input-group",
                    style := "padding-left: 0.75rem; padding-right: 0.75rem",
                    input(
                      `type` := "text",
                      `class` := "form-control",
                      style := "background-color: #fff; text-overflow: ellipsis",
                      placeholder := skillUsage.name,
                      disabled := true,
                    ),
                    div(
                      `class` := "input-group-append",
                      span(
                        `class` := "input-group-text",
                        s"${skillUsage.mpCost} MP"
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
    val demons = rxSelectedDemons()
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
        val moves: Seq[Move] = Seq(Pass(), Attack()) ++ skills
        val selectedAction = rxSelectedAction()
        val buttons = moves.zipWithIndex.map { case (move, index) => {
          val buttonId = s"turn${turn}_${index}"
          val enoughMp = mp >= move.mpCost
          val selected = selectedAction.isSameAction(move)

          val buttonColor = move.name match {
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

          // Use selectedAction instead of move, if we're selected, since move is the base move without crit/miss.
          val (moveName, postscript) = if (selected) {
            (selectedAction.name, selectedAction.postscript)
          } else {
            (move.name, move.postscript)
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
                if (selected) {
                  // Cycle to the next usageType.
                  rxAction() = selectedAction.nextType
                } else {
                  rxAction() = move
                }
              }}: js.ThisFunction),
              moveName,
              if (move.mpCost != 0) { br },
              if (move.mpCost != 0) { s"${move.mpCost} MP" },
              if (postscript.isDefined) { br },
              if (postscript.isDefined) { postscript.get },
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

  val topRight = Rx {
    div(
      style := "padding-right: 0.75rem",
      div(
        a(
          href := s"#${rxConfiguration.serialize().compress()}",
          s"Permalink"
        ),
      ),
      div(
        "Turn order: ",
        onclick := { () => {
          rxConfiguration.first() = !rxConfiguration.first()
          false
        }},
        a(
          href := "#",
          if (rxConfiguration.first()) "first" else "second"
        )
      ),
    )
  }

  def getSerializedConfiguration() = dom.document.location.hash match {
    case "#example" => {
      val ishtar = SerializedDemonConfiguration(
        Some(Dx2Plan.db.demons("Ishtar").id),
        Archetype.Yellow,
        divine = true, lead = false,
        None, None,
        List(
          SkillUsage(SkillInstance(Dx2Plan.db.skills("Concentrate"), true)),
          Attack(),
          SkillUsage(SkillInstance(Dx2Plan.db.skills("Mesopotamian Star"), false)),
          Attack(),
        ),
      )
      val fenrir = SerializedDemonConfiguration(
        Some(Dx2Plan.db.demons("Fenrir").id),
        Archetype.Purple,
        divine = false, lead = false,
        Some(Dx2Plan.db.skills("Rakunda").id), Some(Dx2Plan.db.skills("Makarakarn").id),
        List(Pass(), Pass(), Pass(), Pass()),
      )
      val pyro = SerializedDemonConfiguration(
        Some(Dx2Plan.db.demons("Pyro Jack").id),
        Archetype.Yellow,
        divine = false, lead = false,
        Some(Dx2Plan.db.skills("Megido").id), None,
        List(
          SkillUsage(SkillInstance(Dx2Plan.db.skills("Tag"), true)),
          SkillUsage(SkillInstance(Dx2Plan.db.skills("Tag"), true)),
          SkillUsage(SkillInstance(Dx2Plan.db.skills("Tag"), true)),
          SkillUsage(SkillInstance(Dx2Plan.db.skills("Megido"), false)),
        )
      )
      val jack = SerializedDemonConfiguration(
        Some(Dx2Plan.db.demons("Jack Frost").id),
        Archetype.Yellow,
        divine = false, lead = false,
        None, None,
        List(
          SkillUsage(SkillInstance(Dx2Plan.db.skills("Tag"), true)),
          SkillUsage(SkillInstance(Dx2Plan.db.skills("Tag"), true)),
          SkillUsage(SkillInstance(Dx2Plan.db.skills("Tag"), true)),
        )
      )

      Some(
        SerializedConfiguration(
          demonConfigurations = Map(
            (ConfigurationId(0) -> ishtar),
            (ConfigurationId(1) -> fenrir),
            (ConfigurationId(2) -> pyro),
            (ConfigurationId(3) -> jack)
          ),
          first = true
        )
      )
    }

    case "" => None
    case "#" => None

    case hash => {
      SerializedConfiguration.fromCompressedBytes(hash.substring(1))
    }
  }

  def main(args: Array[String]): Unit = {
    val demonConfigurationElements = (0 until maxDemonCount) map {
      i => generateDemonConfiguration(ConfigurationId(i), serializedConfiguration)
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
            topRight
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
