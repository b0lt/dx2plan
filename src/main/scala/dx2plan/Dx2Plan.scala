package dx2plan

import java.util.Base64

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

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
                              actions: List[Var[Move]] = List.tabulate(Dx2Plan.maxTurnsPerDemon)(_ => Var[Move](Pass)))

case class SerializedDemonConfiguration(demon: String, color: String, divine: Boolean, lead: Boolean,
                                        actions: List[String]) {
  def applyToConfig(config: DemonConfiguration) {
    config.demon() = Demon.find(demon)
    config.color() = Color.deserialize(color)
    config.divine() = divine
    config.lead() = lead
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
    val actions = config.actions.map { action => action().serialize() }
    SerializedDemonConfiguration(demon, color, divine, lead, actions)
  }
}

object DemonConfiguration {
  def serialize(config: DemonConfiguration)(implicit ctx: Ctx.Owner, data: Ctx.Data): SerializedDemonConfiguration = {
    SerializedDemonConfiguration.fromConfig(config)
  }

  def serialize(config: Map[DemonId, DemonConfiguration])(implicit ctx: Ctx.Owner, data: Ctx.Data): String = {
    val bytes = writeBinary(config.map { case (key, value) => (key -> serialize(value)) }.toMap)
    Base64.getUrlEncoder().encodeToString(bytes)
  }

  def deserialize(data: String, config: Map[DemonId, DemonConfiguration]) = {
    val bytes = Base64.getUrlDecoder().decode(data)
    val serializedConfigs = readBinary[Map[DemonId, SerializedDemonConfiguration]](bytes)
    serializedConfigs.foreach { case (id, serializedConfig) => {
      serializedConfig.applyToConfig(config(id))
    }}
  }
}

@JSExport
object Dx2Plan {
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

  val rxGameStates: Seq[Rx[GameState]] = {
    val lb = ListBuffer[Rx[GameState]]()
    var lastState: Option[Rx[GameState]] = None
    (0 until maxDemonCount * maxTurnsPerDemon).foreach { n =>
      lastState match {
        case None => {
          val initialState = Rx {
            val demons = rxDemons()
            val mp = demons.map { case (index, configuration) => (index, initialMp) }.toMap
            val ordering = rxOrdering()
            val firstDemonId = ordering(0)
            val divine = demons(firstDemonId).divine()

            GameState(n, demons.size, mp).regenMp(firstDemonId, divine)
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
            val divine = demons(demonId).divine()

            val previousDemonId = ordering((n - 1) % demons.size)
            val previousDemonTurn = (n - 1) / demons.size
            val rxPreviousDemonMove = demons(previousDemonId).actions(previousDemonTurn)
            prev.evaluate(previousDemonId, rxPreviousDemonMove()).regenMp(demonId, divine)
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
      val skills = ListBuffer[Move]()
      configuration.demon() foreach { demon =>
        demon.baseSkills.foreach { skill =>
          skills += Spell(skill.name, skill.cost.getOrElse(0))
        }
        demon.awakenSkills(configuration.color()) match {
          case Some(skill) => {
            skills += Spell(skill.name, skill.cost.getOrElse(1) - 1)
          }
          case None => {}
        }
      }
      skills.toList
    })
  }}).toMap

  def generateDemonConfiguration(demonId: DemonId) = {
    val idx = demonId.id
    val demonNameId = s"demon${idx}";
    val demonArchetypeId = s"demon${idx}Archetype";
    val demonDivineId = s"demon${idx}Divine";
    val demonLeadId = s"demon${idx}Lead";
    val demon = rxConfigurations(demonId)
    (
      Rx {
        input(
          id := demonNameId,
          autofocus := idx == 0,
          tabindex := idx * 10 + 1,
          autocomplete := "false",
          value := demon.demon().map(_.name).getOrElse(""),
          oninput := ({(elem: HTMLInputElement) => {
            rxConfigurations(demonId).demon() = Demon.find(elem.value)
          }}: js.ThisFunction)
        )
      },
      Rx {
        select(
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
            if (demon.color() == Color.Clear) selected := true,
            "Clear"),
          option(
            value := "red",
            if (demon.color() == Color.Red) selected := true,
            "Red"
          ),
          option(
            value := "yellow",
            if (demon.color() == Color.Yellow) selected := true,
            "Yellow"
          ),
          option(
            value := "purple",
            if (demon.color() == Color.Purple) selected := true,
            "Purple"
          ),
          option(
            value := "teal",
            if (demon.color() == Color.Teal) selected := true,
            "Teal"
          ),
        )
      },
      Rx {
        input(
          id := demonDivineId,
          tabindex := idx * 10 + 3,
          `type` := "checkbox",
          if (demon.divine()) checked := true,
          onchange := ({(elem: HTMLInputElement) => {
            rxConfigurations(demonId).divine() = elem.checked
          }}: js.ThisFunction)
        )
      },
      Rx {
        input(
          id := demonLeadId,
          tabindex := idx * 10 + 4,
          `type` := "checkbox",
          if (demon.lead()) checked := true,
          onchange := ({(elem: HTMLInputElement) => {
            rxConfigurations(demonId).lead() = elem.checked
          }}: js.ThisFunction)
        ),
      },
      Rx {
        rxDemonSkills(demonId)() map {
          case Spell(name, cost) => {
            if (cost == 0) {
              span(s"$name: Passive", br)
            } else {
              span(s"$name: $cost MP", br)
            }
          }
        }
      }
    )
  }

  val moveConfigurationTable = Rx {
    val demons = rxDemons()
    val ordering = rxOrdering()
    require(demons.size == ordering.size)
    val turnCount = demons.size * maxTurnsPerDemon
    val rows = (0 until turnCount) map { turn => {
      val round = turn / ordering.size
      val demonOrder = turn % ordering.size
      val demonId = ordering(demonOrder)

      val demon = demons(demonId)
      require(!demon.demon().isEmpty)

      val demonName = demon.demon().get.name

      val rxGameState = rxGameStates(turn)
      val mp = rxGameState().demonMp(demonId)
      val pressTurns = rxGameState().pressTurns
      if (pressTurns > 0) {
        val rxSelectedAction = demon.actions(round)
        val rxSkills = rxDemonSkills(demonId)
        val skills = rxSkills() filter { skill => skill.mpCost > 0 }
        val moves: Seq[Move] = Seq(Pass, Attack) ++ skills
        val selectedAction = rxSelectedAction()
        val buttons = moves.zipWithIndex.map { case (move, index) => {
          val buttonId = s"turn${turn}_${index}"
          td(
            div(
              style := "text-align:center",
              p(
                input(
                  name := s"turn${turn}",
                  id := buttonId,
                  `type` := "radio",
                  value := move.serialize(),
                  if (selectedAction == move) checked := true,
                  onchange := ({(elem: HTMLInputElement) => {
                    val move = Move.deserialize(elem.value)
                    val demon = rxConfigurations(demonId)
                    demon.actions(round)() = move
                  }}: js.ThisFunction)
                ),
                label(
                  `for` := buttonId,
                  move.name
                )
              ),
              if (move.mpCost != 0) {
                val textStyle = if (move.mpCost > mp) {
                  "color:red"
                } else {
                  ""
                }
                p(
                  style := textStyle,
                  s"${move.mpCost} MP"
                )
              }
            )
          )
        }}

        Some(
          tr(
            th(
              p(demonName),
              p(s"$mp MP"),
              p(s"$pressTurns press turns"),
            ),
            buttons,
          )
        )
      } else {
        None
      }
    }}

    table(rows.filter(!_.isEmpty).map(_.get))
  }

  val permalink = Rx {
    div(
      a(
        href := s"#${DemonConfiguration.serialize(rxConfigurations)}",
        s"Permalink"
      ),
    )
  }

  @JSExport
  def main(): Unit = {
    dom.document.body.innerHTML = ""

    if (!dom.document.location.hash.isEmpty) {
      val serializedData = dom.document.location.hash.substring(1)
      DemonConfiguration.deserialize(serializedData, rxConfigurations)
    }

    val demonConfigurationElements = (0 until maxDemonCount) map { i => generateDemonConfiguration(DemonId(i)) }
    dom.document.body.appendChild(
      section(
        table(
          tr(
            th("Demon"),
            demonConfigurationElements.map(elements => td(elements._1))
          ),
          tr(
            th("Archetype"),
            demonConfigurationElements.map(elements => td(elements._2))
          ),
          tr(
            th("Divine Brand"),
            demonConfigurationElements.map(elements => td(elements._3))
          ),
/*        TODO: Actually implement sorting that takes lead brands into account.
          tr(
            th("Lead Brand"),
            demonConfigurationElements.map(elements => td(elements._4))
          ),
 */
          tr(
            th("Skills"),
            demonConfigurationElements.map(elements => td(elements._5))
          ),
        )
      ).render
    )

    dom.document.body.appendChild(section(moveConfigurationTable).render)
    dom.document.body.appendChild(section(permalink).render)
  }
}
