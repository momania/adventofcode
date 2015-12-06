import scala.annotation.tailrec
import scala.io.Source

object day03 extends App {

  val input = Source.fromFile("day03.input").mkString

  case class Position(x: Int, y: Int) {
    def goNorth = Position(x, y + 1)
    def goWest = Position(x - 1, y)
    def goSouth = Position(x, y - 1)
    def goEast = Position(x + 1, y)
  }
  def computeNextDelivery(direction: Char, currentPosition: Position): Position = {
    direction match {
      case '^' => currentPosition.goNorth
      case '<' => currentPosition.goWest
      case 'v' => currentPosition.goSouth
      case '>' => currentPosition.goEast
    }
  }

  @tailrec def computeDeliveries(directions: List[Char],
                                 currentPosition: Position = Position(0, 0),
                                 deliveries: List[Position] = Nil): List[Position] = {
    directions match {
      case Nil => deliveries
      case direction :: rest =>
        val nextPosition = computeNextDelivery(direction, currentPosition)
        computeDeliveries(rest, nextPosition, nextPosition :: deliveries)
    }
  }

  val allDeliveries = computeDeliveries(input.toList)

  println(s"Deliveries: ${allDeliveries.distinct.size}")

  // #### part 2

  val (santaDirections, roboDirections) = input.toList.zipWithIndex.partition{ case (d, i) => i % 2 == 0}
  val santaDeliveries = computeDeliveries(santaDirections.map(_._1))
  val roboDeliveries = computeDeliveries(roboDirections.map(_._1))

  println(s"With robo deliveries: ${(santaDeliveries ::: roboDeliveries).distinct.size}")
}
