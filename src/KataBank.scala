import scala.io.Source._

object KataBank {
  def main(args: Array[String]) {

    val lines = readLinesFromFile("./src/KataBank.testcases")

    toFlattenedDigitRows(lines)
      .foreach(x => println(x))

    //println(tryParseNumber(Array(' ', ' ', ' ', '|', '_', '|', ' ', ' ', '2')))
  }

  def toAccountNumberDigits(rows: Array[Array[Char]]) {


    def toDigits(input: Array[Char]) = {
      val length = 27
      for (i <- 0 until length by 3)
        yield new Digit(input.take(3) ++ input.drop(length).take(3) ++ input.drop(length * 2).take(3))

        // _  _  _  _  _  _  _  _  _
        //| || || || || || || || || |
        //|_||_||_||_||_||_||_||_||_|

        // _  _  _  _  _  _  _  _  _| || || || || || || || || ||_||_||_||_||_||_||_||_||_|
    }

    rows.map(toDigits)
  }

  def toFlattenedDigitRows(lines: Array[String]) : Array[Array[Char]] = {
    (for (row <- 0 until lines.length by 3)
      yield (lines(row).toString.concat(lines(row + 1)).concat(lines(row + 2))).toCharArray).toArray
  }

  def tryParseNumber(numValues : Array[Char]) : String = {
    val digits = Map(0 -> Array(' ', '_', ' ','|', ' ', '|','|', '_', '|'),
                     1 -> Array(' ', ' ', ' ', ' ', ' ', '|', ' ', ' ', '|'),
                     2 -> Array(' ', '_', ' ', ' ', '_', '|', '|', '_', ' '),
                     3 -> Array(' ', '_', ' ', ' ', '_', '|', ' ', '_', '|'),
                     4 -> Array(' ', ' ', ' ', '|', '_', '|', ' ', ' ', '|'),
                     5 -> Array(' ', '_', ' ', '|', '_', ' ', ' ', '_', '|'),
                     6 -> Array(' ', '_', ' ', '|', '_', ' ', '|', '_', '|'),
                     7 -> Array(' ', '_', ' ', ' ', ' ', '|', ' ', ' ', '|'),
                     8 -> Array(' ', '_', ' ', '|', '_', '|', '|', '_', '|'),
                     9 -> Array(' ', '_', ' ', '|', '_', '|', ' ', '_', '|'))

    val digit = digits.find({
      case (i, x) => x.sameElements(numValues)
    })

    digit match {
      case Some(s) => (s._1).toString
      case None => "?"
    }
  }

  def readLinesFromFile(filename: String) : Array[String] = {
    fromFile(filename).getLines.toArray
  }

  class Digit(input: Array[Char]) {
    var chars: Array[Char] = input

    override def toString() : String = {
      chars.mkString
    }
  }
}
