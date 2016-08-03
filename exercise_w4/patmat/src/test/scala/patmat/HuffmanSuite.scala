package patmat

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

import scala.collection.immutable.Stream.Empty

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t0 = Fork(null,null,Nil,0)
	}

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }
/**
  test("weight of an empty tree") {
    new TestTrees {
      assert(weight(t0) === 0)
    }
  }
*/
    test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }
/**
  test("chars of an empty tree") {
    new TestTrees {
      assert(chars(t0) === List())
    }
  }
*/
  /*test("test for pack") {
    new TestTrees {
      assert(pack(List('a','b','a','c','a')) === List(List('a','a','a'),List('b'),List('c')))
    }
  }*/

  test("the frequency of each character in the text") {
    new TestTrees {
      assert(times(List('a','b','a','c','a')) === List(('a',3),('b',1),('c',1)))
    }
  }

  test("is it a singleton") {
    new TestTrees {
      assert(singleton(List(t1)))
      assert(!singleton(List(t1,t2)))
    }
 }
/**

  test("is it a singleton") {
    new TestTrees {
      assert(singleton(List(t0)))
    }
  }
*/
  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode") {assert(decode(Fork(Fork(Leaf('b', 1), Leaf('a', 1), List('b', 'a'), 2),Leaf('c', 1),List('b', 'a', 'c'), 3), List(0, 0, 0, 1)) === List('b', 'a'))
  }

  test("encode") {
    new TestTrees {
      assert(encode(t2)("abdab".toList) === List(0, 0, 0, 1, 1, 0, 0, 0, 1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
