package adventofcode2024.day2.part2
import scala.io.Source

case class Report(levels: Vector[Int]) {
  private def containsValidLevels(levels: Vector[Int]) = {
    val allAscending = levels == levels.sorted
    val allDescending = levels == levels.sorted.reverse

    val allDiffsValid = levels.take(levels.size - 1)
      .zip(levels.takeRight(levels.size - 1))
      .forall { case (level, nextLevel) => {
        val diff = (nextLevel - level).abs
        diff >= 1 && diff <= 3
      } }
    
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