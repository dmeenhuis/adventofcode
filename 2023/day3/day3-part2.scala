package adventofcode2023.day3.part2
import scala.io.Source
import scala.util.matching.Regex.Match

@main def entryPoint() = {
  val filename = "day3.txt"
  val source = Source.fromFile(filename)
  val lines = source.getLines.toIndexedSeq
  val numbersPattern = "([0-9]+)".r
  val gearPattern = "([\\*]+)".r
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

  def coordinateContainsDigit(coordinate: (Int, Int)): Boolean = {
    val (x, y) = coordinate
    
    charMatrix.apply(y).apply(x).isDigit
  }

  def getPartNumberOnCoordinate(coordinate: (Int, Int)): Int = {
    val (x, y) = coordinate
    val row = charMatrix.apply(y)

    numbersPattern
      .findAllMatchIn(row.mkString)
      .filter(m => m.start <= x && m.end >= x)
      .toList
      .head.matched.toInt
  }

  val gearPartNumbersInFile = charMatrix.zipWithIndex.foldLeft(List[Int]()) { case (partNumbers, (lineChars, index)) =>
    val foundPartNumbersInLine =
      gearPattern
        .findAllMatchIn(lineChars.mkString)
        .map(m =>
          calculateAdjacentCharacterCoordinates(m.start, m.end, index)
            .filter(coordinateContainsDigit)
            .map(getPartNumberOnCoordinate)
            .distinct
        )
        .filter(_.length == 2)
        .foldLeft(List[Int]()) { case (acc, partNumbers) =>
          acc :+ (partNumbers.product)
        }

    partNumbers ++ foundPartNumbersInLine
  }

  println(gearPartNumbersInFile.sum)
}