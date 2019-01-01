package dx2db

import scala.collection.mutable.ListBuffer

object DemonParser {
  def parseDemon(locale: String, demonData: ujson.Obj, races: Map[Int, String]): Option[Demon] = {
    val id = DemonId(demonData("id").num.toInt)
    val name = demonData("name").str
    val description = demonData("desc").str

    // Filter out the Aura Gate bosses.
    if (description == "") {
      return None
    }

    // Filter out items.
    if (demonData("arches").arr.length == 0) {
      return None
    }

    val raceId = demonData("race").num.toInt
    val race = Race(races(raceId), raceId)
    val grade = demonData("grade").num.toInt

    var skills = ListBuffer[SkillId]()
    demonData("skl").arr.foreach {
      case ujson.Obj(skill) => {
        if (skill.value.contains("is_awake")) {
          // Clear awaken skill, skip this and handle it later.
          assert(skill("is_awake").bool == true)
        } else if (skill.value.contains("is_learn")) {
          assert(skill("is_learn").bool == true)
          assert(skill("id").num == 0)
        } else {
          val skillId = skill("id").num.toInt
          if (skillId != 0) {
            skills += SkillId(skillId)
          }
        }
      }
      case _ => return None
    }

    var awakenSkills = Map[Archetype, SkillId]()

    demonData("arches").arr.foreach {
      case ujson.Obj(arch) => {
        val archetype = Archetype.fromJsonId(arch("type").num.toInt)
        val skillId = SkillId(arch("id").num.toInt)
        assert(!awakenSkills.contains(archetype))
        awakenSkills += (archetype -> skillId)
      }
      case _ => return None
    }

    Some(
      Demon(
        id = id,
        names = Map(locale -> name),
        race = race,
        grade = grade,
        description = description,
        stats = Stats.empty(),
        baseSkills = skills.toSeq,
        awakenSkills = awakenSkills,
      )
    )
  }

  def parse(locale: String, dataFiles: LocalizedDataFiles): DemonDb = {
    val json = ujson.read(dataFiles.dataFile2)

    val races = json("dvl_race").arr.map { raceValue =>
      val raceObj = raceValue.obj
      raceObj("id").num.toInt -> raceObj("name").str
    }.toMap

    val demonArray = json("dvl").arr
    val demonBuffer = ListBuffer[(DemonId, Demon)]()

    for (demonData <- demonArray) {
      parseDemon(locale, demonData.obj, races) match {
        case Some(demon) => demonBuffer += (demon.id -> demon)
        case None => {}
      }
    }

    new DemonDb(demonBuffer.toMap)
  }

  def combineNames(base: DemonDb, other: DemonDb) = {
    val namedDemons = base.demons.map { case (demonId, demon) =>
      other.get(demonId) match {
        case Some(otherDemon) => {
          // Do some sanity checking.
          if (demon.race != otherDemon.race ||
              demon.grade != otherDemon.grade ||
              demon.baseSkills != otherDemon.baseSkills ||
              demon.awakenSkills != otherDemon.awakenSkills) {
            System.err.println(s"Base: ${demon.name}\nOther: ${otherDemon.name}\n")
            System.err.println(s"Base: ${demon.race}\nOther: ${otherDemon.race}\n")
            System.err.println(s"Base: ${demon.baseSkills}\nOther: ${otherDemon.baseSkills}\n")
            System.err.println(s"Base: ${demon.awakenSkills}\nOther: ${otherDemon.awakenSkills}\n")
            throw new RuntimeException(s"Mismatch for demon ID $demonId")
            System.exit(1)
          }

          (demonId -> demon.copy(names = demon.names ++ otherDemon.names))
        }

        case None => {
          System.err.println(s"warning: failed to find localized name for ${demon.name}")
          (demonId -> demon)
        }
      }
    }

    DemonDb(namedDemons)
  }

  def parse(dataFiles: DataFiles): DemonDb = {
    val en = parse("en", dataFiles.en)
    val jp = parse("jp", dataFiles.jp)
    combineNames(en, jp)
  }
}
