import scala.io.Source

object day06 extends App {

  val input = Source.fromFile("day06.input").mkString

  val booleanGrid = List.fill(1000)(false).map{_ => List.fill(1000)(false)}

  val instructions = input.split('\n')

  val InstructionLine = "(.*) (\\d+),(\\d+) through (\\d+),(\\d+)".r

  def walkGrid[T](baseGrid: List[List[T]], on: T => T, off: T => T, toggle: T => T): List[List[T]] = {
    instructions.foldLeft(baseGrid) { case (grid, instruction) =>
      val InstructionLine(task, a, b, x, y) = instruction

      val verticalRange = a.toInt to x.toInt
      val horizontalRange = b.toInt to y.toInt

      verticalRange.foldLeft(grid) { case (vg, i) =>
        horizontalRange.foldLeft(vg) { case (hg, j) =>
          val row = hg(i)
          val current = row(j)
          val updatedRow = task match {
            case "turn on" => row.updated(j, on(current))
            case "turn off" => row.updated(j, off(current))
            case "toggle" => row.updated(j, toggle(current))
          }
          hg.updated(i, updatedRow)
        }
      }
    }
  }

  val processedBooleanGrid = walkGrid[Boolean](booleanGrid, _ => true, _ => false, x => !x )

  println(s"Lights on: ${processedBooleanGrid.flatten.count(identity)}")


  // #### step 2

  val numberGrid = List.fill(1000)(0).map(_ => List.fill(1000)(0))

  val processedNumberGrid = walkGrid[Int](numberGrid, _ + 1, x => math.max(x - 1, 0), _ + 2)

  println(s"Total brightness: ${processedNumberGrid.flatten.sum}")
}
