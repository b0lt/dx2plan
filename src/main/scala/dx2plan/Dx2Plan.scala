package dx2plan

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLInputElement, HTMLSelectElement}

import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import rx._

case class DemonSelection(demon: Var[Option[Demon]] = Var[Option[Demon]](None),
                          color: Var[Color] = Var[Color](Color.Clear),
                          divine: Var[Boolean] = Var[Boolean](false),
                          lead: Var[Boolean] = Var[Boolean](false))

sealed trait Move {
  def mpCost: Int
  def pressTurnCost: Double
}

case object Pass extends Move {
  def mpCost = 0
  def pressTurnCost = 0.5
}

case object Attack extends Move {
  def mpCost = 0
  def pressTurnCost = 0.5
}


@JSExport
object Dx2Plan {
  import Framework._
  import Ctx.Owner.Unsafe._

  val selections = (0 until 4).map { _ => DemonSelection() }
  val ordering = Rx {
    val selectedDemons = selections.zipWithIndex.filter { case (selection, index) =>
        !selection.demon().isEmpty
    }
    println(selectedDemons)
    val zipped = selectedDemons.map { case (selection, index) => {
      val agi = selection.demon().get.stats(Stat.Agility)
      val lead = selection.lead()
      (index, agi, lead)
    }}
    val agiSorted = zipped.sortWith((left, right) => left._2 > right._2)

    // TODO: Handle lead brands.
    agiSorted.map { case (index, agi, lead) => index }
  }

  def skillDescriptions(demon: Option[Demon], color: Color) = {
    var text = ListBuffer[String]()
    val skills = demon foreach { d =>
      d.baseSkills.foreach { skill =>
        val description = skill.cost match {
          case Some(mp) => s"${skill.name}: ${mp} MP"
          case None => s"${skill.name}: Passive"
        }
        text += description
      }
      var skills = d.baseSkills
      d.awakenSkills(color) match {
        case Some(skill) => {
          val description = skill.cost match {
            case Some(mp) => s"${skill.name}: ${mp - 1} MP"
            case None => s"${skill.name}: Passive"
          }
          text += description
        }

        case None => {}
      }
    }
    text.toList
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
          selections(idx).demon() = Demon.find(elem.value)
        }}: js.ThisFunction)
      ),
      select(
        id := demonArchetypeId,
        tabindex := idx * 10 + 2,
        oninput := ({(elem: HTMLSelectElement) => {
          selections(idx).color() = elem.value match {
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
          selections(idx).divine() = elem.checked
        }}: js.ThisFunction)
      ),
      input(
        id := demonLeadId,
        tabindex := idx * 10 + 4,
        `type` := "checkbox",
        onchange := ({(elem: HTMLInputElement) => {
          selections(idx).lead() = elem.checked
        }}: js.ThisFunction)
      ),
      Rx { skillDescriptions(selections(idx).demon(), selections(idx).color()).map(p(_)) }
    )
  }

  @JSExport
  def main(): Unit = {
    dom.document.body.innerHTML = ""

    val demonSelectionElements = (0 until 4) map generateDemonSelection
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
          tr(
            th("Lead Brand"),
            demonSelectionElements.map(elements => td(elements._4))
          ),
          tr(
            th("Skills"),
            demonSelectionElements.map(elements => td(elements._5))
          ),
          tr(
            th("Ordering"),
            td(Rx {
              val demonOrdering = ordering().map(idx => selections(idx).demon().get.name)
              demonOrdering.mkString(", ")
            })
          )
        )
      ).render
    )
  }
}
