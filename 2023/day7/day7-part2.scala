package adventofcode2023.day7.part2
import scala.io.Source

val cardValues2 = Vector('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J').reverse.zipWithIndex.toMap

case class CardHandWithJokers(cards: Vector[Char], bid: Int) extends Ordered[CardHandWithJokers] {
  def getType() = {
    val sameCardCounts = cards.filter(_ != 'J').groupBy(identity).toVector.map(_._2.length).sorted.reverse
    val jokersCount = cards.count(_ == 'J')
    val mostCardsCount = if (sameCardCounts.length >= 1) then sameCardCounts(0) else 0
    val secondMostCardsCount = if (sameCardCounts.length > 1) then sameCardCounts(1) else 0

    if (mostCardsCount == 5 || mostCardsCount + jokersCount == 5) then 7                                      // five of a kind
    else if (mostCardsCount == 4 || mostCardsCount + jokersCount == 4) then 6                                 // four of a kind
    else if ((mostCardsCount == 3 || mostCardsCount + jokersCount == 3) && secondMostCardsCount == 2) then 5  // full house
    else if (mostCardsCount == 3 || mostCardsCount + jokersCount == 3) then 4                                 // three of a kind
    else if (mostCardsCount == 2 && secondMostCardsCount == 2) then 3                                         // two pair
    else if (mostCardsCount == 2 || mostCardsCount + jokersCount == 2) then 2                                 // one pair
    else 1
  }

  def compare (that: CardHandWithJokers) = {
    def byCardValues() = cards.zip(that.cards).foldLeft(0){ case (sortValue, (cardA, cardB)) => {
      if (sortValue == 0) cardValues2(cardA).compareTo(cardValues2(cardB)) else sortValue
    }}

    val compared = getType().compareTo(that.getType())

    if compared == 0 then byCardValues() else compared
  }
}

@main def entryPoint() = {
  val input = Source.fromFile("day7.txt").getLines().toVector
  val games2 = input.foldLeft(Vector[CardHandWithJokers]()) { case (acc, line) => {
    val parts = line.split(' ').toVector
    acc :+ CardHandWithJokers(parts(0).toVector, parts(1).toInt)
  }}

  println(games2.sorted.zip(Stream.from(1)).map { case (hand, rank) => {
    // println(s"${hand.cards.mkString} (sorted: ${hand.cards.sortWith((a, b) => cardValues2(a).compareTo(cardValues2(b)) > 0).mkString}): rank: $rank, type: ${hand.getType()}")
    hand.bid * rank
  }}.sum)
}