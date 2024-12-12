package adventofcode2024.day8
import scala.io.Source

object Day8 extends App:
  case class Pos(x: Int, y: Int) {
    def +(that: Pos): Pos = Pos(x + that.x, y + that.y)
    def -(that: Pos): Pos = Pos(x - that.x, y - that.y)
  }

  case class Antenna(frequency: Char, position: Pos)

  val input = Source.fromFile("day8.txt").getLines().map(line => line.toVector).toVector
  val allCoords = for { y <- input.indices; x <- input(y).indices } yield Pos(x, y)

  def isWithinBounds(pos: Pos) = pos.x >= 0 && pos.y >= 0 && pos.x < input(0).size && pos.y < input.size

  val antennas = allCoords
    .filter { case Pos(x, y) => input(y)(x) != '.' }
    .map { case Pos(x, y) => Antenna(input(y)(x), Pos(x, y)) }

  def calculateAntinodes(depth: Int, withResonantHarmonics: Boolean = false) =
    antennas.groupBy(_.frequency).values.foldLeft(Set[Pos]()) { case (antinodes, antennas) => {
      antinodes ++ antennas.combinations(2).foldLeft(antinodes){ case (innerAntinodes, IndexedSeq(a, b)) => {
        def subtract(p: Pos, d: Pos, count: Int = 0): Set[Pos] = if (count < depth) Set(p - d) ++ subtract(p - d, d, count + 1) else Set()
        def add(p: Pos, d: Pos, count: Int = 0): Set[Pos] = if (count < depth) Set(p + d) ++ add(p + d, d, count + 1) else Set()
        
        val diff = a.position - b.position
        val antinodes = subtract(b.position, diff) ++ add(a.position, diff)
        val antennaCoords = if (withResonantHarmonics) antennas.map(_.position) else Set()

        innerAntinodes ++ antinodes ++ antennaCoords
      } }
    }}.filter(isWithinBounds)

  println(s"Part 1 size: ${calculateAntinodes(1).size}")
  println(s"Part 2 size: ${calculateAntinodes(input(0).size, true).size}")