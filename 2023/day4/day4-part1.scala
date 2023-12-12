package adventofcode2023.day4.part1
import scala.io.Source

case class CardGame(number: Int, cardNumbers: Vector[Int], winningNumbers: Vector[Int]) {
  val matching = winningNumbers.intersect(cardNumbers).length
  val points = if (matching == 0) then 0 else Math.pow(2, matching - 1).toInt
}

// cardNumbers.grouped(3).toVector.map(_.trim.toInt)
def parseNumbers(input: String) = input.split(' ').filter(!_.isBlank).map(_.toInt).toVector

@main def entryPoint() = {
  val input = Source.fromFile("day4-test.txt").getLines()
  val cards = input.map:
    case s"Card $cardNum: $winningNumbers | $cardNumbers" =>
      CardGame(cardNum.trim.toInt, parseNumbers(cardNumbers), parseNumbers(winningNumbers))
  
  println(cards.toList)
  val totalPoints = cards.map(_.points).sum

  println(totalPoints)
}