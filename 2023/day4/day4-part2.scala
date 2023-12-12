package adventofcode2023.day4.part2
import scala.io.Source

case class CardGame(number: Int, cardNumbers: Vector[Int], winningNumbers: Vector[Int]) {
  val matching = winningNumbers.intersect(cardNumbers).length
  val points = if (matching == 0) then 0 else Math.pow(2, matching - 1).toInt
}

// cardNumbers.grouped(3).toVector.map(_.trim.toInt)
def parseNumbers(input: String) = input.split(' ').filter(!_.isBlank).map(_.toInt).toVector

@main def entryPoint() = {
  val input = Source.fromFile("day4.txt").getLines()
  val cards = input.map {
    case s"Card $cardNum: $winningNumbers | $cardNumbers" =>
      CardGame(cardNum.trim.toInt, parseNumbers(cardNumbers), parseNumbers(winningNumbers))
  }.toList

  val initialCopies = Vector.fill(cards.length)(1L)
  val copies = cards.zipWithIndex.foldLeft(initialCopies):
    case (acc, (card, index)) =>
      (1 to card.matching).foldLeft(acc)( (innerAcc, i) => innerAcc.updated(index + i, innerAcc(index + i) + innerAcc(index)))
  
  println(copies.sum)
}
