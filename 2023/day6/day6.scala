package adventofcode2023.day6.part1
import scala.io.Source

// Time:      7  15   30
// Distance:  9  40  200

case class RaceAttempt(speed: Int, distance: Int)

@main def entryPoint() = {
  val input = Source.fromFile("day6.txt").getLines().toVector
  val (time, distance) = input.foldLeft((Vector[Int](), Vector[Int]())) { case ((accTime, accDistance), line) => {
    line match {
      case s"Time: $timeEntries" => ("([\\d]+)".r.findAllMatchIn(timeEntries).map(_.matched.toInt).toVector, accDistance)
      case s"Distance: $distanceEntries" => (accTime, "([\\d]+)".r.findAllMatchIn(distanceEntries).map(_.matched.toInt).toVector)
    }
  }}

  val numberOfWinsProduct = time.zip(distance).map { (time, distance) => {
    (0 to time).count(elapsedTime => RaceAttempt(elapsedTime, (time - elapsedTime) * elapsedTime).distance > distance)
  }}.product

  println(numberOfWinsProduct)

  val (time2, distance2) = (42686985L, 284100511221341L)
  println((0L to time2).count(elapsedTime => (time2 - elapsedTime) * elapsedTime > distance2))
}