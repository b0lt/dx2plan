package dx2db

import scala.collection.mutable.ListBuffer

import ujson.Js

object DemonParser {
  def parseDemon(demonData: Js.Obj, races: Map[Int, String]): Option[Demon] = {
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

    val race = races(demonData("race").num.toInt)
    val grade = demonData("grade").num.toInt

    var skills = ListBuffer[SkillId]()
    demonData("skl").arr.foreach {
      case Js.Obj(skill) => {
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

    var archetypeSkills = Map[Archetype, SkillId]()

    demonData("arches").arr.foreach {
      case Js.Obj(arch) => {
        val archetype = Archetype.fromJsonId(arch("type").num.toInt)
        val skillId = SkillId(arch("id").num.toInt)
        assert(!archetypeSkills.contains(archetype))
        archetypeSkills += (archetype -> skillId)
      }
      case _ => return None
    }

    Some(
      Demon(
        id = id,
        name = name,
        jpName = None,
        race = race,
        grade = grade,
        description = description,
        baseSkills = skills.toSeq,
        archetypeSkills = archetypeSkills,
        rawData = demonData
      )
    )
  }

  def parse(dataFiles: LocalizedDataFiles): DemonDb = {
    val json = ujson.read(dataFiles.dataFile2)

    val races = json("dvl_race").arr.map { raceValue =>
      val raceObj = raceValue.obj
      raceObj("id").num.toInt -> raceObj("name").str
    }.toMap

    val demonArray = json("dvl").arr
    val demonBuffer = ListBuffer[(DemonId, Demon)]()

    for (demonData <- demonArray) {
      parseDemon(demonData.obj, races) match {
        case Some(demon) => demonBuffer += (demon.id -> demon)
        case None => {}
      }
    }

    new DemonDb(demonBuffer.toMap)
  }

  def parse(dataFiles: DataFiles): DemonDb = parse(dataFiles.en)
}
