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
      numbers.zipAll(ops, 0L, "").flatMap { case (num, op) => Seq(num.toString, op) }
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
  
  val part1Calibrations = input.map {
    case s"$testValue: ${numbers}" =>
      Calibration(testValue.toLong, numbers.split(" ").map(_.toLong).toVector, Vector("+", "*"))
  }
  val part1 = part1Calibrations.filter(_.isValid()).map(_.testValue).sum
  println(part1)

  val part2Calibrations = input.map {
    case s"$testValue: ${numbers}" =>
      Calibration(testValue.toLong, numbers.split(" ").map(_.toLong).toVector, Vector("+", "*", "||"))
  }
  val part2 = part2Calibrations.filter(_.isValid()).map(_.testValue).sum
  println(part2)
}