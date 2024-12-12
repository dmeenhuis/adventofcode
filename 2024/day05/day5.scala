package adventofcode2024.day5
import scala.io.Source

case class Rule(first: Int, last: Int)

case class Update(pageNumbers: Vector[Int]) {
  def getMiddlePageNumber() = pageNumbers((pageNumbers.size / 2))

  def applyRules(rules: Vector[Rule]) = {
    def process(input: Vector[Int]) = rules.foldLeft((input)) { case (pageNums, Rule(first, last)) => {
      val firstIndex = pageNums.indexOf(first)
      val lastIndex = pageNums.indexOf(last)

      (firstIndex, lastIndex) match {
        case (-1, _) => pageNums
        case (_, -1) => pageNums
        case (f, l) if f > l => pageNums.updated(f, last).updated(l, first)
        case _ => pageNums
      }
    }}

    var previous = pageNumbers
    var current = process(pageNumbers)

    while (current != previous) {
      previous = current
      current = process(current)
    }

    current
  }
}

case class Rules(rules: Vector[Rule]) {
  def isUpdateValid(update: Update) = {
    rules.map { case Rule(first, last) => {
      val firstIndex = update.pageNumbers.indexOf(first)
      val lastIndex = update.pageNumbers.indexOf(last)

      firstIndex == -1 || lastIndex == -1 || firstIndex < lastIndex
    }}.forall(_ == true)
  }

  def invalidUpdates(updates: Vector[Update]) = updates.filter(update => !isUpdateValid(update))
  def validUpdates(updates: Vector[Update]) = updates.filter(isUpdateValid)
}

@main def main() = {
  val (ruleLines, updates) = Source.fromFile("day5.txt").getLines().foldLeft((Vector[Rule](), Vector[Update]())) { case ((accRules, accUpdates), line) => {
    line match {
      case s"$first|$last" => (accRules :+ Rule(first.toInt, last.toInt), accUpdates)
      case "" => (accRules, accUpdates)
      case updates => (accRules, accUpdates :+ Update(updates.split(",").map(_.toInt).toVector))
    }
  }}

  val rules = Rules(ruleLines)

  def part1() = {
    println(rules.validUpdates(updates).map(_.getMiddlePageNumber()).sum)
  }

  def part2() = {
    println(rules.invalidUpdates(updates).map(update => Update(pageNumbers = update.applyRules(rules.rules))).map(_.getMiddlePageNumber()).sum)
  }

  part1()
  part2()
}