import scala.collection.immutable.Nil
object list{
  val data=List('a', 'a', 'b','a','c','a')
  val num=List(1, 1, 2, 6, 7 , 8, 7, 8, 2)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x::xs1 =>
      val (first,rest)= xs partition (y => y==x)
      first::pack(rest)
  }

  pack(num)
}

val test: List[Nil.type] = List(Nil)