package adventofcode2024.day12
import scala.io.Source
import scala.collection.mutable

object Main extends App:
  case class Plot(pos: Pos, perimeter: Int)
  case class Region(plotType: Char, plots: Set[Plot]) {
    def totalPrice() = plots.toVector.map(plot => plot.perimeter).sum * plots.size
    def totalPriceWithDiscount() = sides * plots.size
    def plotExistsAt(p: Pos) = plots.exists(_.pos == p)

    val sides = plots.toVector.map(plot => {
      val allOffsets = plot.pos.allOffsets

      val isTopLeft = (!plotExistsAt(allOffsets(1)) && !plotExistsAt(allOffsets(3))) ||
        (plotExistsAt(allOffsets(1)) && plotExistsAt(allOffsets(3)) && !plotExistsAt(allOffsets(6)))
      val isTopRight = (!plotExistsAt(allOffsets(0)) && !plotExistsAt(allOffsets(3))) ||
        (plotExistsAt(allOffsets(0)) && plotExistsAt(allOffsets(3)) && !plotExistsAt(allOffsets(7)))
      val isBottomLeft = (!plotExistsAt(allOffsets(1)) && !plotExistsAt(allOffsets(2))) ||
        (plotExistsAt(allOffsets(1)) && plotExistsAt(allOffsets(2)) && !plotExistsAt(allOffsets(4)))
      val isBottomRight = (!plotExistsAt(allOffsets(0)) && !plotExistsAt(allOffsets(2))) ||
        (plotExistsAt(allOffsets(0)) && plotExistsAt(allOffsets(2)) && !plotExistsAt(allOffsets(5)))

      Vector(isTopLeft, isTopRight, isBottomLeft, isBottomRight).count(identity)
    }).sum
  }

  val input: Grid[Char] = Grid.unit:
    Source.fromFile("day12.txt").getLines.map(_.toVector).toVector
  
  def getRegions() = {
    val visited: Vector[(Int, Int)] = Vector()
    def loop(pos: Pos, region: Set[Plot]): Set[Plot] = {
      val offsets = input.offsets(pos)
      offsets.foldLeft(region){ case (acc, p) => {
        if (acc.exists(_.pos == p)) acc else {
          val n = input.offsets(p)
          loop(p, acc + Plot(p, 4 - n.size))
        }
      }} + Plot(pos, 4 - offsets.size)
    }

    input.iterateWithIndex.foldLeft(Vector.empty[Region]) { case (acc, (x, y, plotType)) => {
      if (acc.exists(_.plots.exists(_.pos == Pos(x, y)))) {
        acc
      } else {
        val plots = loop(Pos(x, y), Set())
        acc :+ Region(plotType, plots)
    }} }
  }

  val allRegions = getRegions()

  println(s"Part 1: ${allRegions.map(_.totalPrice()).sum}")
  println(s"Part 2: ${allRegions.map(_.totalPriceWithDiscount()).sum}")


opaque type Grid[A] = Vector[Vector[A]]

object Grid:
  def unit[A](grid: Vector[Vector[A]]): Grid[A] = grid

  extension [A](self: Grid[A])
    def get(x: Int, y: Int): A = self(y)(x)

    def liftPos(x: Int, y: Int): Option[A] =
      for
        y <- self.lift(y)
        x <- y.lift(x)
      yield x

    def offsets(p: Pos): Vector[Pos] = p.axisOffsetsFn(n => self.liftPos(n.x, n.y).contains(self.get(p.x, p.y)))

    def iterate: Iterator[A] =
      for
        y <- self.iterator
        x <- y.iterator
      yield x

    def iterateWithIndex: Iterator[(Int, Int, A)] =
      for
        (r, y) <- self.iterator.zipWithIndex
        (c, x) <- r.iterator.zipWithIndex
      yield (x, y, c)

case class Pos(x: Int, y: Int):
  def axisOffsets: Vector[Pos] = Vector(
    Pos(x + 1, y),
    Pos(x - 1, y),
    Pos(x, y + 1),
    Pos(x, y - 1)
  )

  def axisOffsetsFn(f: Pos => Boolean): Vector[Pos] = axisOffsets.filter(f)

  def diagonalOffsets: Vector[Pos] =
    Vector(
      Pos(x - 1, y + 1),
      Pos(x + 1, y + 1),
      Pos(x - 1, y - 1),
      Pos(x + 1, y - 1)
    )

  def allOffsets: Vector[Pos] = axisOffsets ++ diagonalOffsets