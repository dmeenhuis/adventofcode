package adventofcode2023.day3.part1
import scala.io.Source
import scala.util.matching.Regex.Match

def isSymbol(c: Char): Boolean = !c.isDigit && c != '.'

@main def entryPoint() = {
  val filename = "day3.txt"
  val source = Source.fromFile(filename)
  val lines = source.getLines.toIndexedSeq
  val numbersPattern = "([0-9]+)".r
  val numRows = lines.length
  val numCols = lines.head.length
  val charMatrix = List.tabulate(numRows, numCols)( (x, y) => lines.apply(x).charAt(y) )

  def calculateAdjacentCharacterCoordinates(start: Int, end: Int, currentRowIndex: Int) = {
    val colStart = if (start == 0) 0 else start - 1
    val colEnd = if (end == numCols) numCols else end + 1
    val rowStart = if (currentRowIndex == 0) 0 else currentRowIndex - 1
    val rowEnd = if (currentRowIndex == numRows - 1) numRows - 1 else currentRowIndex + 1
    val colRange = (colStart until colEnd)
    val rowRange = (rowStart to rowEnd)

    rowRange.zipWithIndex.foldLeft(List[(Int, Int)]()) { case (acc, (y, rowIndex)) => acc ++ colRange.map(x => (x, y)) }
  }

  def coordinateContainsSymbol(coordinate: (Int, Int)) = {
    val (x, y) = coordinate
    isSymbol(charMatrix.apply(y).apply(x))
  }

  val partNumbersInFile = charMatrix.zipWithIndex.foldLeft(List[Int]()) { case (partNumbers, (lineChars, index)) =>
    val foundPartNumbersInLine =
      numbersPattern
        .findAllMatchIn(lineChars.mkString)
        .map(m => (m.matched.toInt, calculateAdjacentCharacterCoordinates(m.start, m.end, index)))
        .filter((_, coords) => coords.exists(coordinateContainsSymbol))
        .foldLeft(List[Int]()) { case (acc, (partNumber, _)) => acc :+ partNumber }

    partNumbers ++ foundPartNumbersInLine
  }

  println(partNumbersInFile.sum)
}