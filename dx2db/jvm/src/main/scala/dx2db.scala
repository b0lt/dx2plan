package dx2db

object Dx2Db extends App {
  lazy val dataFiles = DataFiles.fromResources().get

  // This is a bit disgusting. The data files from the game are incomplete (stats for level 1, missing gacha skills),
  // so we parse both the English and Japanese versions of the game files to get the Japanese names, and then use the
  // Japanese names to scrape Altema for the information we're missing.
  lazy val altema = AltemaParser.parse(dataFiles)
  lazy val fullSkills = SkillParser.parse(dataFiles)
  lazy val rawDemons = DemonParser.parse(dataFiles)
  lazy val demons = altema match {
    case Some(alt) => alt.augment(rawDemons)
    case None => {
      System.err.println("warning: failed to augment demon db with altema db")
      rawDemons
    }
  }

  lazy val skills = {
    val usedSkills = demons.flatMap {
      case (_, demon) => {
        demon.baseSkills ++ demon.archetypeSkills.values
      }
    }.toSet

    val filtered = fullSkills.filter {
      case (skillId, skill) => {
        if (usedSkills.contains(skillId)) {
          // Used by a demon.
          true
        } else {
          // Gacha/PvP only skills.
          skillId.value match {
            // Deadly Curse
            case 102127 => true

            // Master Assassin
            case 102120 => true

            // Mana Surge
            case 103206 => true

            // Agilao/Bufula/Zanma/Zionga/Hamaon/Mudoon L
            case 1201 | 1202 | 1203 | 1204 | 1205 | 1206 => true

            // Diara L
            case 2028 => true

            // Fatal Sword L
            case 1030 => true

            case _ => false
          }
        }
      }
    }

    filtered
  }
}
