package adventofcode2024.day6
import scala.io.Source

case class Pos(x: Int, y: Int) {
  def +(that: Pos): Pos =
    Pos(x + that.x, y + that.y)
}

val directions: Map[Pos, Pos] =
  Map(
    Pos(0, -1) -> Pos(1, 0),
    Pos(1, 0)  -> Pos(0, 1),
    Pos(0, 1)  -> Pos(-1, 0),
    Pos(-1, 0) -> Pos(0, -1)
  )

case class PuzzleMap(input: Vector[Vector[Char]]) {
  def findInitialGuardPosition(): Pos = {
    input.flatten.find(_ == '^').map(char => {
      val y = input.indexWhere(_.contains(char))
      val x = input(y).indexOf(char)
      Pos(x, y)
    }).get
  }

  def optionalCharAt(x: Int, y: Int): Option[Char] = {
    if (x < 0 || y < 0 || x >= input(0).size || y >= input.size) {
      None
    } else {
      Some(input(y)(x))
    }
  }

  def walk(startPosition: Pos, initialDirection: Pos): Set[(Pos)] = {
    def loop(pos: Pos, dir: Pos, visited: Set[(Pos, Pos)]): Set[(Pos)] = {
      if (visited.contains((pos -> dir))) {
        Set()
      } else {
        val nextPos = pos + dir

        optionalCharAt(nextPos.x, nextPos.y) match {
          case Some('#') => loop(pos, directions(dir), visited)
          case Some(c) => loop(nextPos, dir, visited + (pos -> dir))
          case _ => visited.map((p, _) => p) + pos
        }
      }
    }

    loop(startPosition, initialDirection, Set())
  }

}

@main def main() = {
  val input = Source.fromFile("day6.txt").getLines().map(line => line.toVector).toVector
  val map = PuzzleMap(input)
  val guardPosition = map.findInitialGuardPosition()
  val initialDirection = Pos(0, -1)

  val part1 = map.walk(guardPosition, initialDirection).size
  println(part1)

  val part2 = map.walk(guardPosition, initialDirection).toVector.map {
    case Pos(x, y) => {
      PuzzleMap(input.updated(y, input(y).updated(x, '#'))).walk(guardPosition, initialDirection).size
    }
  }.count(_ == 0)

  println(part2)
}