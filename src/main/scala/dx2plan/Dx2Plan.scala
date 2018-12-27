package dx2plan

import scala.collection.mutable.ListBuffer

import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLInputElement, HTMLSelectElement}
import scalatags.JsDom.all._

import scalatags.JsDom.tags2.section
import rx._
import scala.scalajs.js.annotation.JSExport

case class DemonSelection(demon: Var[Option[Demon]] = Var[Option[Demon]](None),
                          color: Var[Color] = Var[Color](Color.Clear),
                          divine: Var[Boolean] = Var[Boolean](false),
                          lead: Var[Boolean] = Var[Boolean](false))

@JSExport
object Dx2Plan {
  import Framework._

  var selections = (0 until 4).map { _ => DemonSelection() }

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
    import Ctx.Owner.Unsafe._

    val demonNameId = s"demon${idx}Name";
    val demonArchetypeId = s"demon${idx}Archetype";
    val demonDivineId = s"demon${idx}Divine";
    val demonLeadId = s"demon${idx}Lead";
    (
      input(
        id := demonNameId,
        autofocus := idx == 0,
        tabindex := idx * 10 + 1,
        oninput := { () => {
          val elem = dom.document.getElementById(demonNameId)
          selections(idx).demon() = Demon.find(elem.asInstanceOf[HTMLInputElement].value)
        } }
      ),
      select(
        id := demonArchetypeId,
        tabindex := idx * 10 + 2,
        oninput := { () => {
          val elem = dom.document.getElementById(demonArchetypeId)
          selections(idx).color() = elem.asInstanceOf[HTMLSelectElement].value match {
            case "clear" => Color.Clear
            case "red" => Color.Red
            case "yellow" => Color.Yellow
            case "purple" => Color.Purple
            case "teal" => Color.Teal
          }
        } },
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
        onchange := { () => {
          val elem = dom.document.getElementById(demonDivineId)
          selections(idx).divine() = elem.asInstanceOf[HTMLInputElement].value == "on"
        } }
      ),
      input(
        id := demonLeadId,
        tabindex := idx * 10 + 4,
        `type` := "checkbox",
        onchange := { () => {
          val elem = dom.document.getElementById(demonLeadId)
          selections(idx).lead() = elem.asInstanceOf[HTMLInputElement].value == "on"
        } }
      ),
      Rx { skillDescriptions(selections(idx).demon(), selections(idx).color()).map(p(_)) }
    )
  }

  @JSExport
  def main(): Unit = {
    import Ctx.Owner.Unsafe._
    dom.document.body.innerHTML = ""

    val demonSelectionElements = (0 until 4) map generateDemonSelection
    dom.document.body.appendChild(
      section(
        table(
          tr(
            th("Name"),
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
        )
      ).render
    )
  }
}
