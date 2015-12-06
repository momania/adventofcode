import scala.annotation.tailrec
import scala.io.Source

object day01 extends App {

  val input = Source.fromFile("day01.input").mkString

  def movement(c: Char): Int = c match {
    case '(' => 1
    case ')' => -1
  }
  val steps = input.map(movement)

  println(s"Floor: ${steps.sum}")

  // #### part 2

  @tailrec def findBasementIndex(input: List[Char], currentFloor: Int = 0, index: Int = 1): Int = {
    input match {
      case Nil => throw new Exception("Exhausted input without going to the basement!")
      case c :: rest =>
        val floor = currentFloor + movement(c)
        if (floor == -1) {
          index
        } else {
          findBasementIndex(rest, floor, index + 1)
        }
    }
  }
  println(s"Basement index: ${findBasementIndex(input.toList)}")
}
