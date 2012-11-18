package recfun
import common._
import com.sun.org.apache.xalan.internal.xsltc.compiler.ForEach

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    balance_(chars, 0)

  def balance_(xs: List[Char], a: Int): Boolean =
    if (xs.isEmpty) a == 0
    else if (a < 0) false else {
      val i = if (xs.head == '(') 1 else if (xs.head == ')') -1 else 0
      balance_(xs.tail, a + i)
    }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0 || coins.isEmpty || !coins.exists(money % _ == 0)) 0
    else countChange_(money, 0, coins.sortWith(_ > _).distinct.toArray)

  def countChange_(a: Int, i: Int, c: Array[Int]): Int =
    if (a == 0 || i + 1 == c.length) if (a % c(i) == 0) 1 else 0
    else (0 to a / c(i)).map(x => countChange_(a - x * c(i), i + 1, c)).sum
}
