/**
 * Created by dmeenhuis on 3-4-2014.
 */
object FizzBuzz {
  def main(args: Array[String]) {

    (1 until 100).map(FizzBuzz2)

  }

  def FizzBuzz(num: Int) {
    num match {
      case x if (x % 5 == 0 && x % 3 == 0) => println("FizzBuzz")
      case x if (x % 3 == 0) => println("Fizz")
      case x if (x % 5 == 0) => println("Buzz")
      case _ => println(num)
    }
  }

  def FizzBuzz2(num: Int) {
    num match {
      case x if (x % 5 == 0 && x % 3 == 0) => println("FizzBuzz")
      case x if (x % 3 == 0 || containsChar(num, '3')) => println("Fizz")
      case x if (x % 5 == 0 || containsChar(num, '5')) => println("Buzz")
      case _ => println(num)
    }
  }

  def containsChar(num: Int, c: Char) : Boolean = {
    num.toString()
       .toCharArray()
       .contains(c)
  }
}
