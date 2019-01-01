package dx2db

import upickle.default.{ReadWriter => RW, macroRW}

// TODO: Localize this as well?
case class Race(name: String, id: Int) {
  override def equals(other: Any): Boolean = {
    other match {
      case Race(_, otherId) => id == otherId
      case otherName: String => name == otherName
      case _ => false
    }
  }

  override def hashCode(): Int = id.hashCode()
}

object Race {
  implicit val rw: RW[Race] = macroRW
}
