package adventofcode2024.day10
import scala.io.Source


object Main extends App:
  case class Pos(x: Int, y: Int, height: Int)

  val input = Source.fromFile("day10.txt").getLines().map(_.map(_.asDigit).toVector).toVector
  val allPositions = for { y <- input.indices; x <- input(y).indices } yield Pos(x, y, input(y)(x))
  val startingPositions = allPositions.filter(_.height == 0).toVector

  def isWithinBounds(pos: Pos) = pos.x >= 0 && pos.y >= 0 && pos.x < input(0).size && pos.y < input.size
  def getSurroundingPositions(pos: Pos) = Vector((pos.x - 1, pos.y), (pos.x + 1, pos.y), (pos.x, pos.y - 1), (pos.x, pos.y + 1))
    .map((x, y) => allPositions.find(p => p.x == x && p.y == y))
    .flatten
    .filter(isWithinBounds)

  def findTrailHeadSummits(trailheads: Vector[Pos]): Map[Pos, Vector[Pos]] = {
    def loop(start: Pos, current: Pos, summits: Map[Pos, Vector[Pos]]): Map[Pos, Vector[Pos]] = {
        val positionsOneUp = getSurroundingPositions(current).filter(_.height == current.height + 1)
        if (positionsOneUp.isEmpty) summits
        else {
          positionsOneUp.foldLeft(summits)((acc, p) =>
            if (p.height == 9) acc + (start -> (acc.get(start).fold(Vector())(n => n :+ p))) else loop(start, p, acc))
        }
    }

    trailheads.foldLeft(trailheads.map(_ -> Vector[Pos]()).toMap)((acc, pos) => loop(pos, pos, acc))
  }

  val scores = findTrailHeadSummits(startingPositions).map { case (_, value) => value.distinct.size }.sum
  val ratings = findTrailHeadSummits(startingPositions).map { case (_, value) => value.size }.sum

  println(s"Part 1: $scores, part 2: $ratings")
