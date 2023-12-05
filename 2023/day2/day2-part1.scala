package adventofcode2023.day2.part1
import scala.io.Source

case class GameSet(var numRedCubes: Int = 0, var numGreenCubes: Int = 0, var numBlueCubes: Int = 0) {
  def assignValues(color: String, numCubes: Int) = {
    color match {
      case "red" => numRedCubes = numCubes
      case "green" => numGreenCubes = numCubes
      case "blue" => numBlueCubes = numCubes
    }
    this
  }

  def isValid(config: GameConfiguration) = {
    numRedCubes <= config.numRedCubes && numGreenCubes <= config.numGreenCubes && numBlueCubes <= config.numBlueCubes
  }
}

case class PlayedGame(id: Int, sets: Seq[GameSet]) {
  def isValid(config: GameConfiguration) = {
    sets.forall(s => s.isValid(config))
  }
}

case class GameConfiguration(numRedCubes: Int, numGreenCubes: Int, numBlueCubes: Int) {
  def isValidGame(set: GameSet) = {
    set.numRedCubes <= numRedCubes && set.numGreenCubes <= numGreenCubes && set.numBlueCubes <= numBlueCubes
  }
}

def parseGameFromLine(line: String) = {
  // Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  val Array(gameIdData, playedSetsData) = line.split(":").map(_.trim)
  val gameId = "Game ([0-9]+)".r.findFirstMatchIn(gameIdData) match {
    case Some(m) => m.group(1).toInt
    case None => 0
  }

  val gameSetRegex = "([0-9]+) ([a-zA-Z]+)".r
  val sets = playedSetsData.split(";").map(_.trim).map(set => {
    gameSetRegex.findAllMatchIn(set)
      .map(m => (m.group(1).toInt, m.group(2)))
      .foldLeft(new GameSet()) { case (gameSet, (numCubes, color)) =>
        gameSet.assignValues(color, numCubes)
      }
  })

  PlayedGame(gameId, sets)
}

val gameConfig = GameConfiguration(numRedCubes = 12, numGreenCubes = 13, numBlueCubes = 14)

def calculateSumForFile(filename: String) = {
  val source = Source.fromFile(filename)
  source.getLines.map(parseGameFromLine).filter(_.isValid(gameConfig)).foldLeft(0){ case (acc, playedGame) =>
    acc + playedGame.id
  }
}

@main def entryPoint() = {
  println(calculateSumForFile("day2.txt"))
}

