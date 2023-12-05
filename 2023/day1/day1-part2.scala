package adventofcode2023.day1.part2
import scala.io.Source

val wordDigits = List(
  ("one", 1),
  ("two", 2),
  ("three", 3),
  ("four", 4),
  ("five", 5),
  ("six", 6),
  ("seven", 7),
  ("eight", 8),
  ("nine", 9),
)

case class WordMatch(word: String, digit: Int, index: Int)

def replaceLast(input: String, matchOn: String, replaceWith: String) = {
  input.reverse.replaceFirst(matchOn.reverse, replaceWith.reverse).reverse
}

def convertLineToDigits(line: String, direction: String) = {
  wordDigits
    .foldLeft(List[WordMatch]()){ case (acc, (word, digit)) =>
      val matches = word.r.findAllMatchIn(line)
      acc ++ matches.map(m => WordMatch(word, digit, m.start)).toList
    }
    .sortWith((a, b) => if (direction == "ltr") a.index.compareTo(b.index) < 0 else a.index.compareTo(b.index) > 0)
    .foldLeft(line){ case (acc, WordMatch(word, digit, index)) => 
      if (direction == "ltr") acc.replaceFirst(word, digit.toString) else replaceLast(acc, word, digit.toString)
    }
}

def findFirstDigit(charArray: Seq[Char]) = {
  charArray.find(_.isDigit).getOrElse('0').toString
}

def calculateLineCalibrationValue(line: String) = {
  (findFirstDigit(convertLineToDigits(line, "ltr")) +
      findFirstDigit(convertLineToDigits(line, "rtl").reverse)).toInt
}

def calculateCalibrationValueForFile(filename: String) = {
  val source = Source.fromFile(filename)
  source.getLines.foldLeft(0){ case (acc, line) =>
    // println(s"${line},${calculateLineCalibrationValue(line)}")
    // println(convertLineToDigits(line, "ltr"))
    acc + calculateLineCalibrationValue(line)
  }
}

@main def entryPoint() = {
  println(calculateCalibrationValueForFile("day1.txt"))
}