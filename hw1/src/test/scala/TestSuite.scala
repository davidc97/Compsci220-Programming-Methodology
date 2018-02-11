import Lecture1._
class TestSuite extends org.scalatest.FunSuite{

	test("odd numbers properly defined") {
		assert(oddNumbers == List(1, 3, 5))
	}
	test("sumDouble test 1") {
		assert(sumDouble(List(1,2,3)) == 12)
	}

	test("sumDouble test 2") {
		assert(sumDouble(List(10,15,20)) == 90)
	}

	test("countEvens test 1") {
		assert(countEvens(List(1,2,3,4)) == 2)
	}

	test("removeZeroes test 1"){
		assert(removeZeroes(List(1,2,0,3)) == List(1,2,3))
	}

	test("removeAlternating test 1"){
		assert(removeAlternating(List("hello","goodbye","thanks","you're welcome")) == List("hello", "thanks"))
	}

	test("isAscending test true"){
		assert(isAscending(List(1,2,3,4,5)) == true)
	}

	test("isAscending test false"){
		assert(isAscending(List(5,4,3,2,1)) == false)
	}

	test("addSub test"){
		assert(addSub(List(1,2,3,4,5)) == 3)
	}

	test("alternate test"){
		assert(alternate(List(1,3,5), List(2,4,6)) == List(1,2,3,4,5,6))
	}
	test("fromTo test"){
		assert(fromTo(9,13) == List(9,10,11,12))
	}
	test("insertOrdered test"){
		assert(insertOrdered(5, List(1,3,7,9)) == List(1,3,5,7,9))
	}
	test("sort test"){
		assert(sort(List(5,4,3,2,1)) == List(1,2,3,4,5))
	}
}