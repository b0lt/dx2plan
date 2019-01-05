package dx2db

import org.scalajs.dom._

import upickle.default._

object DatabaseFetcher {
  def fetch(url: String): Option[Database] = {
    val xhr = new XMLHttpRequest()
    xhr.open("GET", url, false);
    var result: Option[Database] = None
    xhr.onload = {(e: Event) =>
      if (xhr.status == 200) {
        result = Some(read[Database](xhr.responseText))
      }
    }
    xhr.send()
    result
  }
}
