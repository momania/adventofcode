import scala.io.Source

object day02 extends App {

  val input = Source.fromFile("day02.input").mkString.split('\n')

  val Dimensions = "(\\d+)x(\\d+)x(\\d+)".r

  def computePaper(length: Int, width: Int, height: Int): Int = {
    val x = 2 * length * width
    val y = 2 * width * height
    val z = 2 * length * height
    val surface = Seq(x, y, z)
    surface.sum + (surface.min / 2)
  }

  val paperNeeded = input.map { i =>
    val Dimensions(l, w, h) = i
    computePaper(l.toInt, w.toInt, h.toInt)
  }

  println(s"Paper needed: ${paperNeeded.sum}")

  // #### part 2

  def computeRibbon(length: Int, width: Int, height: Int): Int = {
    val smallestSides = Seq(length, width, height).sorted.reverse.tail
    val base = smallestSides.map{ _ * 2}.sum
    val bow = length * width * height
    base + bow
  }

  val ribbonNeeded = input.map { i =>
    val Dimensions(l, w, h) = i
    computeRibbon(l.toInt, w.toInt, h.toInt)
  }

  println(s"Ribbon needed: ${ribbonNeeded.sum}")
}
