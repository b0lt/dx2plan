package dx2plan

import scala.collection.mutable.ListBuffer

import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLInputElement, HTMLSelectElement}
import scalatags.JsDom.all._

import scalatags.JsDom.tags2.section
import rx._
import scala.scalajs.js.annotation.JSExport

@JSExport
object Dx2Plan {
  import Framework._

  val selectedDemon = Var[Option[Demon]](None)
  val selectedColor = Var[Color](Color.Clear)

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

  @JSExport
  def main(): Unit = {
    import Ctx.Owner.Unsafe._
    dom.document.body.innerHTML = ""
    dom.document.body.appendChild(
      table(
        tr(
          th("Name"),
          td(
            input(
              id := "demonName",
              autofocus := true,
              oninput := { () => {
                val elem = dom.document.getElementById("demonName")
                selectedDemon() = Demon.find(elem.asInstanceOf[HTMLInputElement].value)
              } }
            )
          )
        ),
        tr(
          th("Archetype"),
          td(
            select(
              id := "archetype",
              oninput := { () => {
                val elem = dom.document.getElementById("archetype")
                selectedColor() = elem.asInstanceOf[HTMLSelectElement].value match {
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
            )
          )
        ),
        tr(
          th("Skills"),
          td(
            Rx { skillDescriptions(selectedDemon(), selectedColor()).map(p(_)) }
          )
        )
      ).render
    )
  }
}
