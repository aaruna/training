package recfun
import common._

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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1 else if (c > r/2) pascal(r-c, r) else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
//  def balance(chars: List[Char]): Boolean = {
//    var started: Int = 0
//    for (c <- chars) {
//      if (c == '(') {
//        started += 1
//      } else if (c == ')') {
//        if (started == 0) {
//          return false
//        } else {
//          started -= 1
//        }
//      }
//
//    }
//    return started==0
//  }


  def balance(chars: List[Char]): Boolean = {
    def balanced(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) open == 0
      else
      if (chars.head == '(') balanced(chars.tail,open+1)
      else
      if (chars.head == ')') open>0 && balanced(chars.tail,open-1)
      else balanced(chars.tail,open)
    }
    balanced(chars,0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else
      if (money <= 0) 0
    else
      if (coins.isEmpty) 0
    else
      countChange(money-coins.head, coins) + countChange(money, coins.tail)
  }
}
