package dx2plan

import java.util.Base64

import scala.scalajs.js.JSConverters._

import upickle.default._
import rx._

import dx2db._

// Identifier of demon based on input box layout (i.e. 0 is left-most)
case class ConfigurationId(id: Int)
object ConfigurationId {
  implicit val rw: ReadWriter[ConfigurationId] = macroRW
}

case class DemonConfiguration(demon: Var[Option[Demon]] = Var(None),
                              archetype: Var[Archetype] = Var(Archetype.Clear),
                              divine: Var[Boolean] = Var(false),
                              lead: Var[Boolean] = Var(false),
                              transferSkill0: Var[Option[Skill]] = Var(None),
                              transferSkill1: Var[Option[Skill]] = Var(None),
                              actions: List[Var[Move]] = List.tabulate(Dx2Plan.maxTurnsPerDemon)(_ => Var(Pass)))

case class SerializedDemonConfiguration(demon: Option[DemonId], archetype: Archetype, divine: Boolean, lead: Boolean,
                                        transferSkill0: Option[SkillId], transferSkill1: Option[SkillId],
                                        actions: List[String]) {
  def applyToConfig(config: DemonConfiguration) {
    config.demon() = demon flatMap { Dx2Plan.db.demons.get }
    config.archetype() = archetype
    config.divine() = divine
    config.lead() = lead
    config.transferSkill0() = transferSkill0.flatMap(Dx2Plan.db.skills.get)
    config.transferSkill1() = transferSkill1.flatMap(Dx2Plan.db.skills.get)
    actions.zipWithIndex.foreach { case (action, index) => {
      config.actions(index)() = Move.deserialize(action)
    }}
  }
}

object SerializedDemonConfiguration {
  implicit val rw: ReadWriter[SerializedDemonConfiguration] = macroRW

  def fromConfig(config: DemonConfiguration)(implicit ctx: Ctx.Owner, data: Ctx.Data) = {
    val demon = config.demon().map(_.id)
    val archetype = config.archetype()
    val divine = config.divine()
    val lead = config.lead()
    val transferSkill0 = config.transferSkill0().map(_.id)
    val transferSkill1 = config.transferSkill1().map(_.id)
    val actions = config.actions.map { action => action().serialize() }
    SerializedDemonConfiguration(demon, archetype, divine, lead, transferSkill0, transferSkill1, actions)
  }
}

object DemonConfiguration {
  def serialize(config: DemonConfiguration)(implicit ctx: Ctx.Owner, data: Ctx.Data): SerializedDemonConfiguration = {
    SerializedDemonConfiguration.fromConfig(config)
  }

  def serialize(config: Map[ConfigurationId, DemonConfiguration])(implicit ctx: Ctx.Owner, data: Ctx.Data): String = {
    val bytes = writeBinary(config.map { case (key, value) => (key -> serialize(value)) }.toMap)
    var compressedBytes: Array[Byte] = LZMA.compress(bytes.toJSArray, 1).toArray
    Base64.getUrlEncoder().encodeToString(compressedBytes)
  }

  def deserialize(data: String) = {
    try {
      val bytes = Base64.getUrlDecoder().decode(data)
      var decompressedBytes: Array[Byte] = LZMA.decompress(bytes.toJSArray).toArray
      val serializedConfigs = readBinary[Map[ConfigurationId, SerializedDemonConfiguration]](decompressedBytes)
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
