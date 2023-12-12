package adventofcode2023.day5.part1
import scala.io.Source

case class SourceDestinationMap(sourceRangeStart: Long, destinationRangeStart: Long, rangeLength: Long) {
  def sourceToDestination(source: Long): Option[Long] = {
    if (source >= sourceRangeStart && source <= (sourceRangeStart + rangeLength)) {
      Some(destinationRangeStart + source - sourceRangeStart)
    } else {
      None
    }
  }
}

case class DestinationCategory(source: String, destination: String, maps: Seq[SourceDestinationMap]) {
  def sourceToDestination(source: Long): Long = maps.map(_.sourceToDestination(source)).find(_.isDefined).flatten match {
    case Some(value) => value
    case None => source
  }
}

@main def entryPoint() = {
  val isLineWithNumbers = "^([\\d]+\\s?){1,}$".r
  val input = Source.fromFile("day5-test.txt").getLines().toVector
  
  val (seeds, categories) = input.zipWithIndex
    .foldLeft((Vector[Long](), Vector[DestinationCategory]())) { case ((seeds, categories), (line, index)) =>
      line match {
        case s"seeds: $seedsInput" =>
          (seeds ++ seedsInput.split(' ').map(_.trim.toLong), categories)
        case s"$source-to-$destination map:" =>
          val maps = input.drop(index + 1).takeWhile(isLineWithNumbers.matches).map {
            case s"$destinationRangeStart $sourceRangeStart $rangeLength" =>
              SourceDestinationMap(sourceRangeStart.toLong, destinationRangeStart.toLong, rangeLength.toLong)
          }
          (seeds, categories :+ DestinationCategory(source, destination, maps))
        case _ => (seeds, categories)
      }
    }
  
  val seedLocations = seeds.map((seed) => {
    var category = categories.find(c => c.source == "seed")
    var sourceValue = seed
    var destinationValue = 0L

    while (category != None) {
      destinationValue = category.get.sourceToDestination(sourceValue)
      sourceValue = destinationValue

      category = categories.find(c => c.source == category.get.destination)
    }

    destinationValue
  })
  
  println(seedLocations.min)
}