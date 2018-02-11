import hw.generics._
import ListFunctions._

case class OInt(x: Int) extends Ordered[OInt]{
	def compare(other: OInt): Ordering = {
		if (x > other.get()){
			GT
		} else if ( x < other.get()){
			LT
		} else {
			EQ
		}
	}
	def get(): Int = x
}
class TestSuite extends org.scalatest.FunSuite {
	val mylist1 = new Cons(OInt(0), new Empty())
	val mylist2 = new Cons(OInt(1), new Empty())
	val mylist3 = new Cons(OInt(1), mylist1)
	val mylist4 = new Cons(OInt(0), mylist2)
	val mylist5 = new Cons(OInt(0), mylist3)
	val mylist6 = new Cons(OInt(0), mylist4)
	val tree = Leaf().cons(OInt(1))
	val tree2 = Leaf().cons(OInt(2))
	val tree3 = tree2.cons(OInt(1))
	test("append test"){
		assert(append[OInt,MyList[OInt]](mylist1, mylist2) == mylist4)
	}
	test("append test 2"){
		assert(append[OInt,MyList[OInt]](Empty(), mylist1) == mylist1)
	}
	test("append test 3"){
		assert(append[OInt,BinTree[OInt]](tree,tree2) == tree3)
	}
	test("sort test"){
		assert(sort[OInt,MyList[OInt]](mylist3) == mylist4)
	}
	test("sort test 2"){
		assert(sort[OInt,MyList[OInt]](mylist5) == mylist6)
	}
	test("sort test 3"){
		assert(sort[OInt, BinTree[OInt]](Node(Leaf(), OInt(3), Node(Leaf(), OInt(2), Leaf()))) == Node(Leaf(), OInt(2), Node(Leaf(), OInt(3), Leaf())))
}
	test("sort test 4"){
		assert(sort[OInt, MyList[OInt]](Empty()) == Empty())
	}
}	