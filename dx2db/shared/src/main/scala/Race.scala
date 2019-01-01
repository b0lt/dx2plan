package dx2db

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
