package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

    val msg1 = List('a', 'b')
    val code1 = List(0, 1)
    val msg2 = List('a', 'b', 'b', 'a', 'b', 'b', 'a')
    val code2 = List(0, 1, 1, 0, 1, 1, 0)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times of some char list basic") {
    val charList = List('a', 'b', 'a')
    val timesList = List(('a', 2), ('b', 1))
    assert(times(charList) === timesList)
  }

  test("times of some char list juicier") {
    val charList = List('x', 't', 'e', 'x', 'x', 't', 'x')
    val timesList = List(('x', 4), ('t', 2), ('e', 1))
    assert(times(charList) === timesList)
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("double combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(combine(leaflist)) === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e', 't', 'x'), 7)))
    println(combine(combine(leaflist)).length.toString())
  }

  test("combine until singleton of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton,combine)(leaflist) === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e', 't', 'x'), 7)))
//    println(combine(combine(leaflist)).length.toString())
  }

  test("make ordered leaf list from freq table") {
    val charList = List('x', 't', 'e', 'x', 'x', 't', 'x')
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(makeOrderedLeafList(times(charList)) === leaflist)
  }

  test("decode simplest text") {
    new TestTrees {
      println(decode(t1, code1).toString())
      assert(decode(t1, code1) === msg1)
    }
  }

  test("decode simple text") {
    new TestTrees {
      println(decode(t1, code2).toString())
      assert(decode(t1, code2) === msg2)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode french secret") {
    new TestTrees {
      println(decodedSecret.toString())
    }
  }

  test("convert code tables: t1 and t2") {
    new TestTrees {
      println("convert t1: " + convert(t1))
      assert(convert(t1) === List(('a', List(0)), ('b', List(1))))
      println("convert t2: " + convert(t2))
      assert(convert(t2) === List(('a', List(0,0)), ('b', List(0,1)), ('d', List(1))))
    }
  }

  test("quickie identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)(msg2)) === msg2)
    }
  }
}
