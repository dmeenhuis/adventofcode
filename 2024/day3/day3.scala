package adventofcode2024.day3
import scala.io.Source

@main def main() = {
  val input = Source.fromFile("day3.txt").getLines().mkString

  def part1() = {
    val pattern = "mul\\(([\\d]+),([\\d]+)\\)".r
    pattern.findAllMatchIn(input).map((m) => m.group(1).toInt * m.group(2).toInt).sum
  }
  
  def part2() = {
    val pattern = "^mul\\(([\\d]+),([\\d]+)\\)".r
    var i = 0
    var sum = 0
    var enabled = true

    while (i < input.size) {
      if (i + 4 < input.size && input.substring(i, i + 4) == "do()") {
        enabled = true
        i += 4
      }

      if (i + 7 < input.size && input.substring(i, i + 7) == "don't()") {
        enabled = false
        i += 7
      }

      val matched = pattern.findFirstMatchIn(input.substring(i))

      if (enabled && matched.isDefined) {
        sum += matched.map((m) => m.group(1).toInt * m.group(2).toInt).sum
        i += matched.get.group(0).size - 1
      }

      i += 1
    }

    sum
  }

  println(part1())
  println(part2())
}