package adventofcode2024.day1.part1
import scala.io.Source

@main def main() = {
  val input = Source.fromFile("day1.txt").getLines().toVector.map {
    case s"$a   $b" => (a.toInt, b.toInt)
  }

  val left = input.map(_._1)
  val right = input.map(_._2)
  
  val distance = left.sorted.zip(right.sorted).map { case (a, b) => {
    (a - b).abs
  }}.sum

  println(distance)

  val grouped = right.groupBy(identity).mapValues(_.size).toMap

  val similarityScore = left.map((a) => {
    val rightValue = if (grouped.contains(a)) grouped(a) else 0
    (a * rightValue)
  }).sum

  println(similarityScore)
}