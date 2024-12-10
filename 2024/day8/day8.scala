package adventofcode2024.day8
import scala.io.Source

case class Pos(x: Int, y: Int) {
  def +(that: Pos): Pos =
    Pos(x + that.x, y + that.y)

  def -(that: Pos): Pos =
    Pos(x - that.x, y - that.y)

  def min(that: Pos): Pos =
    Pos(x min that.x, y min that.y)

  def max(that: Pos): Pos =
    Pos(x max that.x, y max that.y)

  def delta(that: Pos): Pos =
    Pos(
      (x max that.x) - (x min that.x),
      (y max that.y) - (y min that.y)
    )

  def sign(that: Pos): Pos =
    Pos((x - that.x).sign, (y - that.y).sign)
}

case class Antenna(frequency: Char, position: Pos)

@main def main() = {
  val input = Source.fromFile("day8.txt").getLines().map(line => line.toVector).toVector
  
  val allCoords = for {
    y <- input.indices
    x <- input(y).indices
  } yield Pos(x, y)

  def isWithinBounds(pos: Pos) = pos.x >= 0 && pos.y >= 0 && pos.x < input(0).size && pos.y < input.size

  val antennaPattern = """^[a-zA-Z0-9]$""".r

  val antennas = allCoords
    .filter { case Pos(x, y) => antennaPattern.matches(input(y)(x).toString) }
    .map { case Pos(x, y) => Antenna(input(y)(x), Pos(x, y)) }

  def calculateAntinodes(depth: Int, includeAntennas: Boolean = false) = {
    val frequencyMap = antennas.groupBy(_.frequency)

    frequencyMap.values.foldLeft(Set[Pos]()) { case (acc, antennas) => {
      acc ++ antennas.combinations(2).foldLeft(acc){ case (innerAcc, pair) => {
        val posA = pair.head.position
        val posB = pair.last.position
        val delta = posA.delta(posB)
        val diff = posA - posB

        def subtractPoints(p: Pos, d: Pos, count: Int = 0): Set[Pos] = count match {
          case num if num < depth => Set(p - d) ++ subtractPoints(p - d, d, count + 1)
          case _ => Set()
        }

        def addPoints(p: Pos, d: Pos, count: Int = 0): Set[Pos] = count match {
          case num if num < depth => Set(p + d) ++ addPoints(p + d, d, count + 1)
          case _ => Set()
        }

        val next = posA.sign(posB) match {
          case Pos(1, 1) => addPoints(posB, diff) ++ subtractPoints(posA, diff)
          case Pos(-1, -1) => addPoints(posB, delta) ++ subtractPoints(posA, delta)
          case Pos(1, -1) => subtractPoints(posB, diff) ++ addPoints(posA, diff)
          case Pos(-1, 1) => subtractPoints(posB, diff) ++ addPoints(posA, diff)
          case _ => Set()
        }

        val antennaCoords = if (includeAntennas) antennas.map(_.position) else Set()

        innerAcc ++ next ++ antennaCoords
      } }
    }}.toVector.filter(isWithinBounds)
  }

  val day1 = calculateAntinodes(1)
  println(s"Day 1 size: ${day1.size}")

  val day2 = calculateAntinodes(input(0).size, true)
  println(s"Day 2 size: ${day2.size}")
}