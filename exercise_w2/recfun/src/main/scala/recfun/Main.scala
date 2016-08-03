package recfun

import scala.annotation.tailrec

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
    if ((c == 0) || (r == 0) || (c == r)) 1 else (pascal(c - 1, r - 1) + pascal(c , r-1))

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def newbalance(count: Int, acc: List[Char], newchars: List[Char]): Boolean = {
      newchars match {
        case Nil => count == 0 && acc.isEmpty
        case head :: tail if head == '(' => newbalance(count + 1, acc :+ '(', tail)
        case head :: tail if head == ')' => newbalance(count - 1, if (acc.isEmpty) acc else acc.tail, tail)
      }
    }

    val newchars: List[Char] = chars.filter(c => c == '(' || c == ')')
    newbalance(0, List.empty, newchars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def change(money: Int, coin: List[Int], acc: Int): Int =
      money match {
        case _ if money < 0 => acc
        case _ if coin.isEmpty => {
          money match {
            case 0 => acc + 1
            case _ => acc
          }
        }
        case _ => change(money, coin.tail, acc) + change(money - coin.head, coin, acc)
      }
    change(money, coins, 0)
  }


}
