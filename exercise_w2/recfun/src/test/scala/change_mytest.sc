/*
object exercise {
  def countChange(money: Int, coins: List[Int]): Int = {
    def mycountChange(acc : Int,  money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) { 0 }
      else if (coins.contains(money)) { 1 }
      else if (money < coins.min) { acc }
      else {
        coins.foreach { e => if(money >= e) { mycountChange(acc+1, money - e, coins) }}
      }
      mycountChange(0, money, coins)
    }
    countChange(4,List(1,2))
  }
}
*/
object exercise {
  def countChange(money: Int, coins: List[Int]): Int = {
    def change(money: Int, coins: List[Int], acc: Int): Int =
      money match {
        case _ if money < 0 => acc
        case _ if coins.isEmpty => {
                    money match {
                      case 0 => acc + 1
                      case _ => acc
                    }
        }
        case _ => change(money, coins.tail, acc) + change(money - coins.head, coins, acc)
      }
    change(money, coins, 0)
  }
  countChange(4,List(1,2))
}