package adventofcode2024.day11
import scala.io.Source
import scala.collection.mutable

object Main extends App:
  val input = Source.fromFile("day11.txt").getLines.mkString.split(' ').map(c => c.toInt).toVector
  val cache = mutable.Map.empty[(Long, Int), Long]

  def blink(number: Int, times: Int): Long = {
    def loop(stone: Long, remaining: Int): Long = (stone, remaining) match {
      case (_, 0) => 1
      case (0, t) => loop(1, t - 1)
      case (s, t) if (s.toString.length % 2 == 0) =>
        val (l, r) = s.toString.splitAt(s.toString.length / 2)
        cache.getOrElseUpdate((l.toInt, t), loop(l.toInt, t - 1)) + cache.getOrElseUpdate((r.toInt, t), loop(r.toInt, t - 1))
      case (s, t) => loop(s * 2024, t - 1)
    }
    loop(number, times)
  }

  println(s"Part 1: ${input.map(blink(_, 25)).sum}")
  println(s"Part 2: ${input.map(blink(_, 75)).sum}")
