package dx2plan

import java.util.Base64

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.JSConverters._

import upickle.default._
import rx._

import dx2db._

// Holder of all state.
case class Configuration(
  demonConfigurations: Map[ConfigurationId, DemonConfiguration] =
    List.tabulate(Dx2Plan.maxDemonCount)(id => ConfigurationId(id) -> DemonConfiguration()).toMap,
  first: Var[Boolean] = Var(true),
) {
  def serialize()(implicit ctx: Ctx.Owner, data: Ctx.Data): SerializedConfiguration = {
    val serializedDemons = demonConfigurations.map { case (key, value) => (key -> value.serialize()) }.toMap
    SerializedConfiguration(serializedDemons, first())
  }
}

object Configuration {
  def fromSerializedConfiguration(serialized: Option[SerializedConfiguration]): Configuration = {
    serialized match {
      case Some(s) => {
        val result = Configuration()
        val assignments = s.applyTo(result)
        Var.set(assignments: _*)
        result
      }

      case None => Configuration()
    }
  }
}

case class SerializedConfiguration(
  demonConfigurations: Map[ConfigurationId, SerializedDemonConfiguration],
  first: Boolean,
) {
  def applyTo(configuration: Configuration) = {
    val assignments = ArrayBuffer[Var.Assignment[_]]()
    demonConfigurations.foreach {
      case (id, demonConfiguration) => {
        assignments ++= demonConfiguration.applyTo(configuration.demonConfigurations(id))
      }
    }
    assignments += Var.Assignment(configuration.first, first)
    assignments.toArray
  }

  def compress(): String = {
    val bytes = writeBinary(this)
    var compressedBytes: Array[Byte] = LZMA.compress(bytes.toJSArray, 1).toArray
    Base64.getUrlEncoder().encodeToString(compressedBytes)
  }
}

object SerializedConfiguration {
  implicit val rw: ReadWriter[SerializedConfiguration] = macroRW

  def fromCompressedBytes(data: String) = {
    try {
      val bytes = Base64.getUrlDecoder().decode(data)
      var decompressedBytes: Array[Byte] = LZMA.decompress(bytes.toJSArray).toArray
      val serializedConfigs = readBinary[SerializedConfiguration](decompressedBytes)
      Some(serializedConfigs)
    } catch {
      case ex: Throwable => {
        println(s"Failed to deserialize data: '$data'")
        ex.printStackTrace()
        None
      }
    }
  }
}

// Identifier of demon based on input box layout (i.e. 0 is left-most)
case class ConfigurationId(id: Int)
object ConfigurationId {
  implicit val rw: ReadWriter[ConfigurationId] = macroRW
}

case class DemonConfiguration(
  demon: Var[Option[Demon]] = Var(None),
  archetype: Var[Archetype] = Var(Archetype.Clear),
  divine: Var[Boolean] = Var(false),
  lead: Var[Boolean] = Var(false),
  transferSkill0: Var[Option[Skill]] = Var(None),
  transferSkill1: Var[Option[Skill]] = Var(None),
  actions: List[Var[Move]] = List.tabulate(Dx2Plan.maxTurnsPerDemon)(_ => Var(Pass()))
) {
  def serialize()(implicit ctx: Ctx.Owner, data: Ctx.Data): SerializedDemonConfiguration = {
    val demon = this.demon().map(_.id)
    val archetype = this.archetype()
    val divine = this.divine()
    val lead = this.lead()
    val transferSkill0 = this.transferSkill0().map(_.id)
    val transferSkill1 = this.transferSkill1().map(_.id)
    val actions = this.actions.map { action => action() }
    SerializedDemonConfiguration(demon, archetype, divine, lead, transferSkill0, transferSkill1, actions)
  }
}

case class SerializedDemonConfiguration(demon: Option[DemonId], archetype: Archetype, divine: Boolean, lead: Boolean,
                                        transferSkill0: Option[SkillId], transferSkill1: Option[SkillId],
                                        actions: List[Move]) {
  def applyTo(config: DemonConfiguration) = {
    val assignments = ArrayBuffer[Var.Assignment[_]]()
    assignments += Var.Assignment(config.demon, demon flatMap { Dx2Plan.db.demons.get })
    assignments += Var.Assignment(config.archetype, archetype)
    assignments += Var.Assignment(config.divine, divine)
    assignments += Var.Assignment(config.lead, lead)

    val skill0 = transferSkill0.flatMap(Dx2Plan.db.skills.get)
    val skill1 = transferSkill1.flatMap(Dx2Plan.db.skills.get)
    assignments += Var.Assignment(config.transferSkill0, skill0)
    assignments += Var.Assignment(config.transferSkill1, skill1)
    actions.zipWithIndex.foreach { case (action, index) => {
      assignments += Var.Assignment(config.actions(index), action)
    }}

    assignments.toArray
  }
}

object SerializedDemonConfiguration {
  implicit val rw: ReadWriter[SerializedDemonConfiguration] = macroRW
}
