package recfun

object Main {

  def main(args: Array[String]): Unit = {

    // Pascal's Triangle
    println(">>> Pascal's Triangle:")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println()

    //Brackets Balance
    println(">>> Brackets Balance:")
    println("(if (zero? x) max (/ 1 x)) => " + balance("(if (zero? x) max (/ 1 x))".toList))
    println("I told him (that it's not (yet) done).\\n(But he wasn't listening) => " +
      balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
    println(":-) => " + balance(":-)".toList))
    println("())( => " + balance("())(".toList))
    println()

    // Count change
    println(">>> Count change:")
    println("Money: 4; Coins: 1, 2. => " + countChange(4, List(1, 2)) + " changes")
    println("Money: 300; Coins: 5, 10, 20, 50, 100, 200, 500. => " +
      countChange(300, List(5, 10, 20, 50, 100, 200, 500)) + " changes")
    println("Money: 301; Coins: 5, 10, 20, 50, 100, 200, 500. => " +
      countChange(301, List(5, 10, 20, 50, 100, 200, 500)) + " changes")
    println("Money: 300; Coins: 500, 5, 50, 100, 20, 200, 10. => " +
      countChange(300, List(500, 5, 50, 100, 20, 200, 10)) + " changes")
    println()

    // Mathematical operations
    println(">>> Sums:")
    println("0\t  + 5 = " + sum(0,5))
    println("10\t  + 0 = " + sum(10,0))
    println("10\t  + 5 = " + sum(10,5))
    println("(-10) + 5 = " + sum(-10,5))
    println("10\t  + (-5) = " + sum(10,-5))
    println("5\t  + (-5) = " + sum(5,-5))
    println("(-10) + (-5) = " + sum(-10,-5))
    println()

    println(">>> Multiplies:")
    println("0\t  * 5 = " + multiply(0,5))
    println("10\t  * 0 = " + multiply(10,0))
    println("10\t  * 5 = " + multiply(10,5))
    println("(-10) * 5 = " + multiply(-10,5))
    println("10\t  * (-5) = " + multiply(10,-5))
    println("(-10) * (-5) = " + multiply(-10,-5))
    println()

    println(">>> Powers:")
    println("0\t ^ 0 = " + power(0, 0))
    println("0\t ^ 4 = " + power(0, 4))
    println("2\t ^ 0 = " + power(2, 0))
    println("0\t ^ (-4) = " + power(0, -4))
    println("(-2) ^ 0 = " + power(-2, 0))
    println("2\t ^ 4 = " + power(2, 4))
    println("(-2) ^ 4 = " + power(-2, 4))
    println("2\t ^ (-4) = " + power(2, -4))
    println("(-2) ^ (-4) = " + power(-2, -4))
    println("(-2) ^ 3 = " + power(-2, -4))
    println("(-2) ^ (-3) = " + power(-2, -4))
    println("2.5\t ^ (-2) = " + power(2.5, -2))
    println()

    // Task_4
    println(">>> Task_4:")
    println("n = 0;\t x = 0;\t\t => c = " + task_4(0, 0))
    println("n = 2;\t x = 2;\t\t => c = " + task_4(2, 2))
    println("n = -2;\t x = -2;\t => c = " + task_4(-2, -2))
    println("n = 0;\t x = 2;\t\t => c = " + task_4(0, 2))
    println("n = 2;\t x = 0;\t\t => c = " + task_4(2, 0))
    println("n = 0;\t x = -2;\t => c = " + task_4(0, -2))
    println("n = -2;\t x = 0;\t\t => c = " + task_4(-2, 0))
    println("n = 0;\t x = 2.5;\t => c = " + task_4(0, 2.5))
    println("n = 0;\t x = -2.5;\t => c = " + task_4(0, -2.5))
    println("n = 2;\t x = -2;\t => c = " + task_4(2, -2))
    println("n = -2;\t x = 2;\t\t => c = " + task_4(-2, 2))
    println("n = 2;\t x = -2.5;\t => c = " + task_4(2, -2.5))
    println("n = -2;\t x = 2.5;\t => c = " + task_4(-2, 2.5))

  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c >= 0 && r >= 0) factorial(r) / factorial(c) / factorial(r - c)
    else 0
  }

  def factorial(n: Int): Int = {
    if (n >= 1) n * factorial(n - 1)
    else 1
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = loop(chars, 0, 0)

  def loop(chars: List[Char], currentIndex: Int, openBrackets: Int): Boolean = {
    if (currentIndex == chars.length) return openBrackets == 0
    if (openBrackets < 0) return false
    if (chars(currentIndex) == '(') return loop(chars, currentIndex + 1, openBrackets + 1)
    if (chars(currentIndex) == ')') return loop(chars, currentIndex + 1, openBrackets - 1)

    loop(chars, currentIndex + 1, openBrackets)
  }

  /**
  * Exercise 3
  */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty ) 0
    else if (money == 0 ) 1
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }

  /**
  * Exercise 4
  */
  def task_4(n: Int, x: Double): Any = {
    if (x < n) power(sum(x, n), n)
    else if (x > n) multiply(n, power(x, n))
    else null
  }

  def sum(a: Double, b: Double): Double = {
    if (b == 0) a
    else if (b.toInt != b && a.toInt != a) 0.1 * sum(10 * a, 10 * b)
    else if (b < 0 && a > 0) sum(b, a)
    else sum(a + 1, b - 1)
  }

  def multiply(a: Double, b: Double): Double = {
    if (a.isInfinite || b.isInfinite) Double.PositiveInfinity
    else if (a == 0) 0
    else if (b == 1.0) a
    else if (b.toInt != b && a.toInt != a) 0.01 * multiply(10 * a, 10 * b)    // a,b є (0; 1)
    else if (b.toInt != b && a < 0)  0.1 * multiply(a, 10 * b)                // b є (0; 1); a < 0
    else if ((b < 0 && a > 0) || (b > 0 && b < 1 && a > 0)) multiply(b, a)    // b є (0; 1); a > 0
    else if (b < 0 && a < 0) multiply(math.abs(a), math.abs(b))
    else sum(a, multiply(a, b - 1))

  }

  def power(a: Double, b: Int): Double = {
    if (b == 0) 1
    else if (b < 0) 1 / power(a, math.abs(b))
    else multiply(a, power(a, b - 1))
  }

}