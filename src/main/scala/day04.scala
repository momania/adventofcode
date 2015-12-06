import java.security.MessageDigest

import scala.annotation.tailrec

object day04 extends App {

  val input = "iwrupvqb"

  val digest = MessageDigest.getInstance("MD5")

  def md5(text: String): String = {
    digest.digest(text.getBytes).map("%02x".format(_)).mkString
  }

  @tailrec def findHashStartingWith(what: String, test: String, number: Int = 1): Int = {
    if (number % 100000 == 0) {
      println(s"Testing at $number...")
    }
    val hash = md5(s"$test$number")
    if (hash.startsWith(what)) {
      number
    } else {
      findHashStartingWith(what, test, number + 1)
    }
  }

  println(s"Five zeros on number: ${findHashStartingWith("00000", input)}")

  // #### part 2

  println(s"Six zeros on number: ${findHashStartingWith("000000", input)}")
}
