package adventofcode2023.day1.part1
import scala.io.Source

def findFirstDigit(charArray: Seq[Char]) = {
  charArray.find(_.isDigit).getOrElse('0').toString
}

def calculateLineCalibrationValue(line: String) = {
  (findFirstDigit(line) + findFirstDigit(line.reverse)).toInt
}

def calculateCalibrationValueForFile(filename: String) = {
  val source = Source.fromFile(filename)
  source.getLines.foldLeft(0){ case (acc, line) =>
    acc + calculateLineCalibrationValue(line)
  }
}

@main def entryPoint() = {
  println(calculateCalibrationValueForFile("day1-part1-test.txt"))
}