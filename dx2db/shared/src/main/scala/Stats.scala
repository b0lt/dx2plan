package dx2db

import upickle.default.{ReadWriter => RW, macroRW}

case class Stats(hp: Int, strength: Int, magic: Int, vitality: Int, agility: Int, luck: Int) {
  private def level = 50
  private def attack(stat: Int) = (stat * 2.1 + level * 5.6 + 50).toInt
  def physicalAttack = attack(strength)
  def magicalAttack = attack(magic)

  private def defense(stat: Int) = (vitality * 1.1 + stat * 0.5 + level * 5.6 + 50).toInt
  def physicalDefense = defense(strength)
  def magicalDefense = defense(magic)

  def isEmpty(): Boolean ={
    (hp, strength, magic, vitality, agility, luck) == (0, 0, 0, 0, 0, 0)
  }
}

object Stats {
  implicit val rw: RW[Stats] = macroRW

  def empty(): Stats = Stats(0, 0, 0, 0, 0, 0)
}
