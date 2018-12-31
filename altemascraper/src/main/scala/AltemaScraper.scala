import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

object AltemaScraper extends App {
  lazy val browser = JsoupBrowser()
  lazy val doc = browser.get("https://altema.jp/megaten/charalist")
  lazy val scripts = (doc >> elementList("script")).map(_.innerHtml)

  // This is a disgusting hack.
  lazy val pattern = raw"""var sortData = (\[.+\]);""".r.unanchored
  lazy val data = scripts.map {
    script: String => script match {
      case pattern(data) => Some(data)
      case _ => None
    }
  }.find(_.isDefined).getOrElse(None)

  data match {
    case Some(string) => {
      Files.write(Paths.get("altema.json"), string.getBytes(StandardCharsets.UTF_8))
    }

    case None => {
      System.err.println("Failed to find demon data")
      System.exit(1)
    }
  }
}
