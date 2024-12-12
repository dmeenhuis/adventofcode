package adventofcode2024.day2
import scala.io.Source

case class Report(levels: Vector[Int]) {
  private def containsValidLevels(levels: Vector[Int]) = {
    val allAscending = levels == levels.sorted
    val allDescending = levels == levels.sorted.reverse
    val allDiffsValid = levels.sliding(2).forall((pair) => 1 to 3 contains (pair(1) - pair(0)).abs)
    
    (allAscending || allDescending) && allDiffsValid
  }

  private def problemDampener() = levels.zipWithIndex.exists { case (level, index) => {
    val dampenedLevels = levels.patch(index, Nil, 1)
    containsValidLevels(dampenedLevels)
  } }

  def isValid() = containsValidLevels(levels)
  def isValidWithDampener() = isValid() || problemDampener()
}

@main def main() = {
  val input = Source.fromFile("day2.txt").getLines().toVector
  val reports = input.map((line) => Report(line.split(" ").map(_.toInt).toVector))

  println(reports.count(_.isValid()))
  println(reports.count(_.isValidWithDampener()))
}