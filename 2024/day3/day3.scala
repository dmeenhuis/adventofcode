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
    val tokenDo = "do()"
    val tokenDont = "don't()"

    while (i < input.size) {
      
      if (i + tokenDo.size < input.size && input.substring(i, i + tokenDo.size) == tokenDo) {
        enabled = true
        i += tokenDo.size
      }

      if (i + tokenDont.size < input.size && input.substring(i, i + tokenDont.size) == tokenDont) {
        enabled = false
        i += tokenDont.size
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