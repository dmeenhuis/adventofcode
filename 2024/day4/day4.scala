package adventofcode2024.day4
import scala.io.Source

@main def main() = {
  val input = Source.fromFile("day4.txt").getLines().map(line => line.toVector).toVector

  val allCoordinatesFromInput = for {
    y <- input.indices
    x <- input(y).indices
  } yield (x, y)

  def safeIndexAt(x: Int, y: Int): Option[Char] = {
    if (x < 0 || y < 0 || x >= input(0).size || y >= input.size) {
      None
    } else {
      Some(input(y)(x))
    }
  }

  def part1() = {
    val word = "XMAS"

    def getXmasCountsForCoordinate(x: Int, y: Int) = {
      val right = input(y).slice(x, x + word.size)
      val left = input(y).splitAt(x + 1)._1.reverse.slice(0, word.size)
      val down = input.slice(y, y + word.size).map(line => line(x))
      val up = input.splitAt(y + 1)._1.reverse.slice(0, word.size).map(line => line(x))
      val topLeft = Vector(safeIndexAt(x, y), safeIndexAt(x - 1, y - 1), safeIndexAt(x - 2, y - 2), safeIndexAt(x - 3, y - 3)).flatten
      val topRight = Vector(safeIndexAt(x, y), safeIndexAt(x + 1, y - 1), safeIndexAt(x + 2, y - 2), safeIndexAt(x + 3, y - 3)).flatten
      val bottomLeft = Vector(safeIndexAt(x, y), safeIndexAt(x - 1, y + 1), safeIndexAt(x - 2, y + 2), safeIndexAt(x - 3, y + 3)).flatten
      val bottomRight = Vector(safeIndexAt(x, y), safeIndexAt(x + 1, y + 1), safeIndexAt(x + 2, y + 2), safeIndexAt(x + 3, y + 3)).flatten
      
      Vector(right, left, down, up, topLeft, topRight, bottomLeft, bottomRight).count(_.mkString == word)
    }

    val wordMatches = allCoordinatesFromInput.map(getXmasCountsForCoordinate).sum
    println(wordMatches)
  }

  def part2() = {
    def getSurroundingCoords(x: Int, y: Int) = {
      for {
        i <- -1 to 1
        j <- -1 to 1
      } yield (x + i, y + j)
    }

    def hasXedMas(x: Int, y: Int) = {
      val coordsToCheck = getSurroundingCoords(x, y).toVector
      val charOptions = coordsToCheck.map(safeIndexAt)
      val leg1 = Vector(charOptions(0), charOptions(4), charOptions(8)).flatten.mkString
      val leg2 = Vector(charOptions(2), charOptions(4), charOptions(6)).flatten.mkString

      List(leg1, leg2).forall(List("MAS", "SAM").contains)
    }

    val wordMatches = allCoordinatesFromInput.filter(hasXedMas).size
    println(wordMatches)
  }

  part1()
  part2()
}