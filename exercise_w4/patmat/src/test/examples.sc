import patmat.Huffman.{Fork, Leaf}

List(1, 2, 4, 3).sortBy(i => i)
List('a','b','a','c','a').sortBy(i => i).toList

type Bit = Int

/**
  * This function decodes the bit sequence `bits` using the code tree `tree` and returns
  * the resulting list of characters.
  */
abstract class CodeTree {
  def chars: List[Char]
}

case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

case class Leaf(char: Char, weight: Int) extends CodeTree {
  override def chars: List[Char] = List(char)
}

def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
  def decode_acc(currentTree: CodeTree, currentBits: List[Bit], msg: List[Char]): List[Char] = {
    currentTree match {
      case _ if currentBits.isEmpty => msg
      case Leaf(char, _) => char :: decode_acc(tree, currentBits.tail, msg)
      case Fork(left, right, _, _) => currentBits.head match {
        case 0 => decode_acc(left, currentBits.tail, msg)
        case 1 => decode_acc(right, currentBits.tail, msg)
      }
    }
  }
  decode_acc(tree, bits, List())
}

decode(Fork(Fork(Leaf('b', 1), Leaf('a', 1), List('b', 'a'), 2), Leaf('c', 1), List('b', 'a', 'c'), 3), List(0, 0, 0, 1))
