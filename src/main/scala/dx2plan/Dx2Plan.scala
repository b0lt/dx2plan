package dx2plan

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLInputElement, HTMLSelectElement}

import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import rx._

// Identifier of demon based on input box layout (i.e. 0 is left-most)
case class DemonId(id: Int)

case class DemonSelection(demon: Var[Option[Demon]] = Var[Option[Demon]](None),
                          color: Var[Color] = Var[Color](Color.Clear),
                          divine: Var[Boolean] = Var[Boolean](false),
                          lead: Var[Boolean] = Var[Boolean](false))

@JSExport
object Dx2Plan {
  import Framework._
  implicit val ctx: Ctx.Owner = Ctx.Owner.safe()

  val maxDemonCount = 4
  val maxTurnsPerDemon = 4
  val initialMp = 2

  // Underlying reactive variables for demon selection.
  val rxSelections: Seq[DemonSelection] = (0 until maxDemonCount).map { _ => DemonSelection() }

  // Actions are stored by demon instead of order, so that if a demon is changed, only that demon's moves are changed.
  val rxActions = (0 until maxDemonCount).map { id =>
    DemonId(id) -> ((0 until maxTurnsPerDemon) map { _ => Var[Move](Pass) })
  }.toMap

  // Actually selected demons.
  val rxDemons = Rx {
    rxSelections.zipWithIndex.filter { case (selection, index) =>
        !selection.demon().isEmpty
    }.map { case (selection, index) =>
      DemonId(index) -> selection
    }.toMap
  }

  // List of DemonIds ordered by turn order.
  val rxOrdering = Rx {
    val demonInfo = rxDemons().map { case (id, selection) => {
      val agi = selection.demon().get.stats(Stat.Agility)
      val lead = selection.lead()
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
            val mp = demons.map { case (index, selection) => (index, initialMp) }.toMap
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
            val rxPreviousDemonMove = rxActions(previousDemonId)(previousDemonTurn)
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
    (DemonId(i) -> Rx {
      val selection = rxSelections(i)
      val skills = ListBuffer[Move]()
      selection.demon() foreach { demon =>
        demon.baseSkills.foreach { skill =>
          skills += Spell(skill.name, skill.cost.getOrElse(0))
        }
        demon.awakenSkills(selection.color()) match {
          case Some(skill) => {
            skills += Spell(skill.name, skill.cost.getOrElse(1) - 1)
          }
          case None => {}
        }
      }
      skills.toList
    })
  }}).toMap

  def skillDescriptions(id: DemonId) = Rx {
    val rxSkills = rxDemonSkills(id)
    rxSkills() map {
      case Spell(name, cost) => {
        if (cost == 0) {
          span(s"$name: Passive", br)
        } else {
          span(s"$name: $cost MP", br)
        }
      }
    }
  }

  def generateDemonSelection(idx: Int) = {
    val demonNameId = s"demon${idx}";
    val demonArchetypeId = s"demon${idx}Archetype";
    val demonDivineId = s"demon${idx}Divine";
    val demonLeadId = s"demon${idx}Lead";
    (
      input(
        id := demonNameId,
        autofocus := idx == 0,
        tabindex := idx * 10 + 1,
        autocomplete := "false",
        oninput := ({(elem: HTMLInputElement) => {
          rxSelections(idx).demon() = Demon.find(elem.value)
        }}: js.ThisFunction)
      ),
      select(
        id := demonArchetypeId,
        tabindex := idx * 10 + 2,
        oninput := ({(elem: HTMLSelectElement) => {
          rxSelections(idx).color() = elem.value match {
            case "clear" => Color.Clear
            case "red" => Color.Red
            case "yellow" => Color.Yellow
            case "purple" => Color.Purple
            case "teal" => Color.Teal
          }
        }}: js.ThisFunction),
        option(value := "clear", "Clear"),
        option(value := "red", "Red"),
        option(value := "yellow", "Yellow"),
        option(value := "purple", "Purple"),
        option(value := "teal", "Teal"),
      ),
      input(
        id := demonDivineId,
        tabindex := idx * 10 + 3,
        `type` := "checkbox",
        onchange := ({(elem: HTMLInputElement) => {
          rxSelections(idx).divine() = elem.checked
        }}: js.ThisFunction)
      ),
      input(
        id := demonLeadId,
        tabindex := idx * 10 + 4,
        `type` := "checkbox",
        onchange := ({(elem: HTMLInputElement) => {
          rxSelections(idx).lead() = elem.checked
        }}: js.ThisFunction)
      ),
      skillDescriptions(DemonId(idx))
    )
  }

  val moveSelectionTable = Rx {
    val demons = rxDemons()
    val ordering = rxOrdering()
    require(demons.size == ordering.size)
    val turnCount = demons.size * maxTurnsPerDemon
    val rows = (0 until turnCount) map { turn => {
      val round = turn / ordering.size
      val demonOrder = turn % ordering.size
      val demonId = ordering(demonOrder)

      val demon = demons(demonId).demon()
      require(!demon.isEmpty)

      val rxGameState = rxGameStates(turn)
      val mp = rxGameState().demonMp(demonId)
      val pressTurns = rxGameState().pressTurns
      if (pressTurns > 0) {
        val rxSelectedAction = rxActions(demonId)(round)
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
                  value := Move.toString(move),
                  if (selectedAction == move) checked := true,
                  onchange := ({(elem: HTMLInputElement) => {
                    val move = Move.fromString(elem.value)
                    rxActions(demonId)(round)() = move
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
              p(demon.get.name),
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

  @JSExport
  def main(): Unit = {
    dom.document.body.innerHTML = ""

    val demonSelectionElements = (0 until maxDemonCount) map generateDemonSelection
    dom.document.body.appendChild(
      section(
        table(
          tr(
            th("Demon"),
            demonSelectionElements.map(elements => td(elements._1))
          ),
          tr(
            th("Archetype"),
            demonSelectionElements.map(elements => td(elements._2))
          ),
          tr(
            th("Divine Brand"),
            demonSelectionElements.map(elements => td(elements._3))
          ),
/*        TODO: Actually implement sorting that takes lead brands into account.
          tr(
            th("Lead Brand"),
            demonSelectionElements.map(elements => td(elements._4))
          ),
 */
          tr(
            th("Skills"),
            demonSelectionElements.map(elements => td(elements._5))
          ),
        )
      ).render
    )

    dom.document.body.appendChild(section(moveSelectionTable).render)
  }
}
