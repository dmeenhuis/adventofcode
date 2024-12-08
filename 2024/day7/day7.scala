package adventofcode2024.day7
import scala.io.Source

case class Calibration(testValue: Long, numbers: Vector[Long], operators: Vector[String]) {
  def isValid() = {
    // Generate all possible combinations of operators
    val operatorCombinations = {
    val operatorCount = numbers.length - 1
    (1 to operatorCount).foldLeft(Vector(Vector.empty[String])) { (acc, _) =>
      for {
        combo <- acc
        op <- operators
      } yield combo :+ op
      } }

    // Combine the numbers with each operator combination
    val combinations = operatorCombinations.map { ops =>
      numbers.zip(ops :+ " ").flatMap {
        case (n, o) => Vector(n.toString, o)
      }.dropRight(1)
    }

    combinations.map(entries => {
      entries.zipWithIndex.foldLeft(0L){ case (acc, (char, index)) => {
        char match {
          case "+" => acc + entries(index + 1).toLong
          case "*" => acc * entries(index + 1).toLong
          case "||" => s"$acc${entries(index + 1)}".toLong
          case num if index == 0 => num.toLong
          case _ => acc
        }
      }}
    }).contains((testValue))
    
  }
}

@main def main() = {
  val input = Source.fromFile("day7.txt").getLines().toVector
  
  val day1Calibrations = input.map {
    case s"$testValue: ${numbers}" =>
      Calibration(testValue.toLong, numbers.split(" ").map(_.toLong).toVector, Vector("+", "*"))
  }
  val day1 = day1Calibrations.filter(_.isValid()).map(_.testValue).sum
  println(day1)

  val day2Calibrations = input.map {
    case s"$testValue: ${numbers}" =>
      Calibration(testValue.toLong, numbers.split(" ").map(_.toLong).toVector, Vector("+", "*", "||"))
  }
  val day2 = day2Calibrations.filter(_.isValid()).map(_.testValue).sum
  println(day2)
}