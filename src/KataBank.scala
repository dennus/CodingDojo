import scala.io.Source._

object KataBank {
  def main(args: Array[String]) {

    val lines = readLinesFromFile("./src/KataBank.testcases")

    toFlattenedDigitRows(lines)
    .foreach(x => println(x))

    //println(new File(".").getAbsolutePath())

    //println(tryParseNumber(Array(' ', ' ', ' ', '|', '_', '|', ' ', ' ', '2')))
  }

  def toAccountNumberDigits(lines: Array[String]) {
    var digits = List()


    for (row <- 0 to lines.length by 3) {
      val line = lines(row)
      var digit = Array()

      for(col <- 0 to line.length by 3) {

      }

     // _  _  _  _  _  _  _  _  _
     //| || || || || || || || || |
     //|_||_||_||_||_||_||_||_||_|

      // _  _  _  _  _  _  _  _  _| || || || || || || || || ||_||_||_||_||_||_||_||_||_|



    }
  }

  def toFlattenedDigitRows(lines: Array[String]) : Array[Array[Char]] = {

    (for (row <- 0 to lines.length by 3)
                  yield (lines(row).toString.concat(lines(row + 1)).concat(lines(row + 2))).toCharArray).toArray
  }

  def tryParseNumber(numValues : Array[Char]) : String = {
    val digits = Map(0 -> Array(' ', '_', ' ',
                                '|', ' ', '|',
                                '|', '_', '|'),
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
}
