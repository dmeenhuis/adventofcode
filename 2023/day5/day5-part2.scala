package adventofcode2023.day5.part2
import scala.io.Source
import scala.collection.parallel.CollectionConverters._
import java.util.concurrent.atomic.AtomicLong
import scala.util.control.Breaks._

// humidity-to-location map:
// 60 56 37
// 56 93 4

case class SourceDestinationMap(sourceRangeStart: Long, destinationRangeStart: Long, rangeLength: Long) {
  def sourceToDestination(source: Long): Option[Long] = {
    if (source >= sourceRangeStart && source <= (sourceRangeStart + rangeLength)) {
      Some(destinationRangeStart + source - sourceRangeStart)
    } else {
      None
    }
  }

  def destinationToSource(destination: Long): Option[Long] = {
    if (destination >= destinationRangeStart && destination < (destinationRangeStart + rangeLength)) {
      Some(sourceRangeStart - destinationRangeStart + destination)
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

  def destinationToSource(destination: Long): Long = maps.map(_.destinationToSource(destination)).find(_.isDefined).flatten match {
    case Some(value) => value
    case None => destination
  }

  val minDestinationValue = maps.minBy(m => m.destinationRangeStart)
}

@main def entryPoint() = {
  val isLineWithNumbers = "^([\\d]+\\s?){1,}$".r
  val input = Source.fromFile("day5.txt").getLines().toVector
  
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
  
  val seedsWithRanges = seeds.grouped(2).map {
    case Vector(seed, range) => (seed, range)
    case _ => sys.error("uneven size")
  }.toList

  // var lowestDestination: AtomicLong = new AtomicLong(0L)

  val resultsMap = scala.collection.mutable.Map[Long, Long]()
  // resultsMap 

  println(seedsWithRanges)
  
  seedsWithRanges.par.foreach((startingSeed, range) => {
    breakable {
      (startingSeed until startingSeed + range).par.foreach(seed => {
      // (55L until 56L).foreach(seed => {
        var foundSeedValue = 0L

        var category = categories.find(c => c.destination == "location")

        var initialLocationValue = 1L
        // println(s"seed: ${seed}")
        
        while (seed != foundSeedValue) {
          
          var category = categories.find(c => c.destination == "location")
          var sourceValue = 0L
          var destinationValue = initialLocationValue

          while (category != None) {
            sourceValue = category.get.destinationToSource(destinationValue)
            // println(s"destination ${category.get.destination} ($destinationValue) -> source ${category.get.source} ($sourceValue)")
            destinationValue = sourceValue

            if (category.get.source == "seed") {
              foundSeedValue = sourceValue
              // println(s"seed required ($seed), found seed value: $foundSeedValue")
            }

            category = categories.find(c => c.destination == category.get.source)
          }

          initialLocationValue = initialLocationValue + 1

          println(s"foundSeedValue: $foundSeedValue")
        }

        resultsMap.put(seed, initialLocationValue - 1)
        break

        // val current = resultsMap.get(startingSeed) match {
        //   case Some(value) => value
        //   case _ => 0L
        // }
        
        // resultsMap.put(startingSeed, if current == 0 then destinationValue else Math.min(current, destinationValue))

        // while (category != None) {
        //   destinationValue = category.get.sourceToDestination(sourceValue)
        //   sourceValue = destinationValue

        //   category = categories.find(c => c.source == category.get.destination)
        // }

        // val current = resultsMap.get(startingSeed) match {
        //   case Some(value) => value
        //   case _ => 0L
        // }
        
        // resultsMap.put(startingSeed, if current == 0 then destinationValue else Math.min(current, destinationValue))

        // lowestDestination = if lowestDestination == 0 then destinationValue else Math.min(lowestDestination, destinationValue)
      })
    }

    // if (acc == 0) then minValue else Math.min(acc, minValue)
  })
  
  println(resultsMap.map((key, value) => value).min)
  // val lowest = destinations.flatMap

  // 49 53 8
  // 0 11 42
  // 42 0 7 
  // 57 7 4

  // val cats = DestinationCategory("fertilizer", "water", List(
  //   SourceDestinationMap(53, 49, 8),
  //   SourceDestinationMap(11, 0, 42),
  //   SourceDestinationMap(0, 42, 7),
  //   SourceDestinationMap(7, 57, 4)
  // ))
  // // destination <= (destinationRangeStart + rangeLength)
  // // 42 < 0 + 42
  // // 42
  // println(cats.destinationToSource(42))
  // // println(SourceDestinationMap(11, 0, 42).destinationToSource(42))
  // println(SourceDestinationMap(11, 0, 42).destinationToSource(42))
  // println(SourceDestinationMap(0, 42, 7).destinationToSource(42))
}