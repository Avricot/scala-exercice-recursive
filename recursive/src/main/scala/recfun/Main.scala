package recfun

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
    * 1
    * 1 1
    * 1 2 1
    * 1 3 3 1
    * 1 4 6 4 1
    *
    */
  def pascal(c: Int, r: Int): Int = {
    if (r <= 1 || c == 0 || c == r)
      1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def count(chars: List[Char], open: Int): Boolean = {
      if (open < 0) return false
      chars match {
        case Nil => open == 0
        case '(' :: tail =>count(tail, open + 1)
        case ')' :: tail =>count(tail, open - 1)
        case _ :: tail  => count(tail, open)
      }
    }
    count(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0)
      1
    else if(money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0
  }
}
