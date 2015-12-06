import scala.io.Source

object day05 extends App {

  val input = Source.fromFile("day05.input").mkString
  val words = input.split('\n')

  val naughtyStrings = List("ab", "cd", "pq", "xy")
  val vowels = List('a', 'e', 'i', 'o', 'u')

  def isNiceWord(word: String): Boolean = {
    val chars = word.toList
    val slides = chars.sliding(2).toList
    val hasThreeVowels = word.count(c => vowels.exists(_ == c)) > 2
    val nonNaughty = slides.forall {
      case List(x, y)=> !naughtyStrings.exists(_ == s"$x$y")
    }
    val containsDouble = slides.exists {
      case List(x, y) => x == y
    }
    hasThreeVowels && nonNaughty && containsDouble
  }

  val niceWords = words.count(isNiceWord)

  println(s"Nice words: $niceWords")

  // #### part 2

  def isEvenNicerWord(word: String): Boolean = {

    val chars = word.toList
    val doubleSlides = chars.sliding(2).toList

    val grouped = doubleSlides.zipWithIndex.groupBy(_._1)
    val multiGroups = grouped.filter(_._2.length > 1)
    println("word = " + word)
    val pairWithoutOverlap = multiGroups.filter { elem =>
      println("elem = " + elem._2)
      val indices = elem._2.map(_._2)
      val zipped = indices.zip(indices.tail)
      println("zipped = " + zipped)
      zipped.map(i => i._2 - i._1).max > 1
    }

    val trippleSlides = chars.sliding(3).toList
    val hasRepeatWithBetween = trippleSlides.exists {
      case List(x, _, z) => x == z
    }

    pairWithoutOverlap.nonEmpty && hasRepeatWithBetween
  }

  val evenNicerWords = words.count(isEvenNicerWord)
  println(s"Even nicer words: $evenNicerWords")
}
