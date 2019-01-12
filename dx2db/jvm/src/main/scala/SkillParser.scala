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
        val result = Target.fromIntValue(active("target").num.toInt)
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

      val levels = {
        if (active.value.contains("skl_levels")) {
          val skl_levels = active.value("skl_levels").arr
          skl_levels.map {
            case skl_level: ujson.Obj => {
              val level = skl_level("level").num.toInt

              def parseSkillEffect(obj: ujson.Obj): SkillLevelEffect = {
                val param1 = obj("param1").num.toInt
                val param2 = obj("param2").num.toInt
                obj("affect_type").num.toInt match {
                  case 1 => {
                    assert(param1 > 0)
                    assert(param2 == 0)
                    SkillLevelEffect.Damage(param1)
                  }

                  case 2 => {
                    assert(param1 > 0)
                    assert(param2 > 0)
                    SkillLevelEffect.ManaRecovery(param1, param2)
                  }

                  case 3 => {
                    assert(param1 > 0)
                    assert(param2 == 0)
                    SkillLevelEffect.AilmentRate(param1)
                  }

                  case 4 => {
                    assert(param1 > 0)
                    assert(param2 == 0)
                    SkillLevelEffect.HealingAmount(param1)
                  }

                  case 5 => {
                    assert(param1 > 0)
                    assert(param2 == 0)
                    SkillLevelEffect.HitRate(param1)
                  }

                  case 6 => {
                    assert(param1 > 0)
                    assert(param2 == 0)
                    SkillLevelEffect.CostReduction(param1)
                  }

                  case _ => ???
                }
              }

              val effects: Seq[SkillLevelEffect] = skl_level("aff_list").arr.map {
                case effect: ujson.Obj => parseSkillEffect(effect)
                case _ => ???
              }

              // The first effect seems to be duplicated for some reason.
              val first = parseSkillEffect(skl_level.obj)
              assert(first == effects(0))
              effects
            }

            case _ => ???
          }
        } else {
          Seq[Seq[SkillLevelEffect]]()
        }
      }

      Skill.Active(id, name, description, element, cost, target, effects, effectCancel, levels)
    } else {
      assert(skillData.value.contains("passive"))
      // TODO: Parse passive levels?
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
