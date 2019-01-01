package dx2db

import scala.collection.mutable.ListBuffer

object SkillParser {
  def parseSkill(skillData: ujson.Obj): Skill = {
    val id = SkillId(skillData("id").num.toInt)
    val name = skillData("name").str
    val description = skillData("desc").str

    if (skillData.value.contains("active")) {
      val active = skillData("active").obj

      val element = {
        val elem = Element.fromBitmask(active("attr").num.toInt)
        assert(elem.size <= 1)
        if (elem.isEmpty) {
          None
        } else {
          Some(elem.head)
        }
      }

      // 0 if Dazed, 1 if Phys, otherwise 2?
      active("category").num.toInt match {
        case 0 => assert(name == "Dazed")
        case 1 => assert(element == Some(Element.Phys))
        case 2 => assert(element != Some(Element.Phys))
        case invalid => assert(false, "Unexpected category " + invalid)
      }

      val cost = active("cost").num.toInt
      assert(active("cost_type").num.toInt == 1)

      val target = {
        val result = Target.fromValue(active("target").num.toInt)
        // Silent Prayer, which is implemented via two chained skills.
        if (id.value == 2020) {
          assert(result == Target.AllAllies)
          Target.Everyone
        } else {
          result
        }
      }

      val effects = {
        if (active.value.contains("good")) {
          Effect.fromJson(active.value("good").obj)
        } else {
          Set[Effect]()
        }
      }

      val effectCancel = {
        if (active.value.contains("good_cancel")) {
          Effect.fromJson(active.value("good_cancel").obj)
        } else {
          Set[Effect]()
        }
      }

      Skill.Active(id, name, description, element, cost, target, effects, effectCancel)
    } else {
      assert(skillData.value.contains("passive"))
      Skill.Passive(id, name, description)
    }
  }

  def parse(dataFiles: DataFiles): SkillDb = {
    val json = ujson.read(dataFiles.en.dataFile3)
    val skillArray = json("dvl_skl").arr
    val skillBuffer = ListBuffer[(SkillId, Skill)]()

    for (skillData <- skillArray) {
      val skill = parseSkill(skillData.obj)
      skillBuffer += (skill.id -> skill)
    }

    new SkillDb(skillBuffer.toMap)
  }
}
