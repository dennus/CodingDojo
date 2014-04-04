import scala.io.Source._
import Functional._

object KataBank {
  def main(args: Array[String]) {

    val accounts = readLinesFromFile("./src/KataBank.testcases") |>
                      toAccountNumbers

    accounts.foreach(x => println(x))
  }

  def toAccountNumberDigits(rows: Array[Array[Char]]) : Array[Account] = {
    def toDigits(input: Array[Char]) : Array[Digit] = {
      val accountLength = 27

      val converted = for (i <- 0 until accountLength by 3)
                        yield {
                          val s1 = input.take(3)
                          val s2 = input.drop(accountLength).take(3)
                          val s3 = input.drop(accountLength * 2).take(3)

                          val cmb = s1 ++ s2 ++ s3

                          tryParseNumber(cmb)
                        }

      converted.toArray
    }

    rows
      .map(toDigits)
      .map(x => {
        new Account(x)
      })
  }

  def toFlattenedDigitRows(lines: Array[String]) : Array[Array[Char]] = {
    val result = for (row <- 0 until lines.length by 4)
                    yield {
                      val r1 = lines(row).toString
                      val r2 = lines(row + 1).toString
                      val r3 = lines(row + 2).toString

                      val c1 = r1.concat(r2)
                      val c2 = c1.concat(r3)

                      c2.toCharArray
                    }

    result.toArray
  }

  def toAccountNumbers(lines: Array[String]) : Array[Account] = {
    val accounts = toFlattenedDigitRows(lines) |>
                      toAccountNumberDigits

    accounts.toArray
  }

  def tryParseNumber(numValues : Array[Char]) : Digit = {
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
      case Some(s) => new ValidDigit(s._1)
      case None => new InvalidDigit()
    }
  }

  def readLinesFromFile(filename: String) : Array[String] = {
    fromFile(filename).getLines.toArray
  }


}

class Account(digitsInput: Array[Digit]) {
  var digits: Array[Digit] = digitsInput

  override def toString() : String = {
    var result = ""

    for(i <- 0 until digits.length) {
      result += digits(i).displayValue
    }

    result
  }
}

trait Digit {
  def displayValue() : String
}

class ValidDigit(inputNumber: Int) extends Digit {
  var number: Int = inputNumber

  def displayValue() : String = {
    number.toString
  }

  override def toString() : String = {
    number.toString
  }
}

class InvalidDigit extends Digit {
  def displayValue() : String = {
    "?"
  }
}
