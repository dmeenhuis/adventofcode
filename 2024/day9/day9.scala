package adventofcode2024.day9
import scala.io.Source

object Main extends App:
  val input = Source.fromFile("day9.txt").getLines().mkString
  val diskMap = input.map(_.asDigit).toVector.zipWithIndex.foldLeft(Vector[Int]()) {
    case (acc, (num, index)) if (index % 2 == 0) => acc ++ Vector.fill(num)((index / 2).toInt)
    case (acc, (num, _)) => acc ++ Vector.fill(num)(-1)
  }

  val defragmented = diskMap.zipWithIndex.foldRight(diskMap) { case ((num, index), acc) => {
    acc.indexOf(-1) match {
      case n if n > -1 => acc.updated(n, num).dropRight(1)
      case _ => acc
    }
  } }
  
  def checksum(input: Vector[Int]) = input.zipWithIndex.foldLeft(0L) { case (acc, (num, index)) => if (num == -1) acc else acc + (num * index) }

  val (compacted, _) = diskMap.zipWithIndex.foldRight((diskMap, 0)) { case ((num, index), (acc, currentIndex)) => {
    if (currentIndex >= diskMap.size - 1) {
      (acc, currentIndex)
    } else {
      if (acc(diskMap.size - currentIndex - 1) == -1) {
        (acc, currentIndex + 1)
      } else {
        val reversed = acc.reverse.drop(currentIndex)
        val first = reversed.head
        val file = reversed.takeWhile(n => n == first).toVector
        val updated = acc.indexOfSlice(Vector.fill(file.size)(-1)) match {
          case n if n > -1 && n < diskMap.size - currentIndex =>
            acc.patch(n, file, file.size).patch(diskMap.size - currentIndex - file.size, Vector.fill(file.size)(-1), file.size)
          case _ => acc
        }
        (updated, currentIndex + file.size)
      }
    }
  } }
  
  println(s"Day 1: ${checksum(defragmented)}")
  println(s"Day 2: ${checksum(compacted)}")
