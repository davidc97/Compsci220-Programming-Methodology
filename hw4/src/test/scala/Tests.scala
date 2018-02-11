class Tests extends org.scalatest.FunSuite {

  import FunctionalDataStructures._

  def fromList[A](lst: List[A]): JoinList[A] = lst match {
    case Nil => Empty()
    case List(x) => Singleton(x)
    case _  => {
      val len = lst.length
      val (lhs, rhs) = lst.splitAt(len / 2)
      Join(fromList(lhs), fromList(rhs), len)
    }
  }

  def toList[A](lst: JoinList[A]): List[A] = lst match {
    case Empty() => Nil
    case Singleton(x) => List(x)
    case Join(lst1, lst2, _) => toList(lst1) ++ toList(lst2)
  }

  test("enqueue test"){
    assert(enqueue("hello", Queue(List(""), List("goodbye"))) == Queue(List(""),List("hello","goodbye")))
  }

  test("dequeue test 1"){
    assert(dequeue(Queue(List(),List(6,5,4))) == Some(4,Queue(List(5,6),List())))
  }

  test("max test empty"){
    assert((max(fromList(List()), (a: Int, b: Int) => (a > b)) == None))
  }
  test("max test 2"){
    assert((max(Join(Singleton(2),Singleton(1),2), (a: Int, b: Int) => (a > b)) == Some(2)))
  }
  test("max test 3"){
    assert((max(fromList(List(1,2,3,4,5,10,3,4,1,5,2)),(a:Int, b: Int) => (a >b)) == Some(10)))
  }
  test("max test 4"){
    assert(max(Singleton(2),(a:Int, b: Int) => (a>b)) == Some(2))
  }
  test("first test 1"){
    assert(first(fromList(List())) == None)
  }
  test("first test 2"){
    assert(first(fromList(List(1,2,3,4))) == Some(1))
  }
  test("first test 3"){
    assert(first(Join(Join(Empty(),Empty(),0),Join(Empty(),Singleton(1),1),1)) == Some(1))
  }
  test("rest test 1"){
    assert(rest(fromList(List())) == None)
  }
  test("rest test 2"){
    assert(Some(toList(rest(fromList(List(1,2,3,4))).get)) == Some(List(2,3,4)))
  }
  test("rest test 3"){
    assert(rest(fromList(List(1))) == Some(fromList(List())))
  }
  test("rest test 4"){
    assert(toList(rest(Join(Empty(),Singleton(1),1)).get) == toList(Join(Empty(),Singleton(1),1)).tail)
  }
  test("rest test 5"){
    assert(Some(toList(rest(Join(Join(Singleton(1),Singleton(2),2),Empty(),2)).get)) == Some(List(2)))
  }
  test("map test 1"){
    assert(map((x:Int) => (x+1), fromList(List(1,2,3,4))) == fromList(List(2,3,4,5)))
  }
  test("map test empty"){
    assert(map((x:Int) => (x+1), fromList(List())) == Empty())
  }
  test("filter test 1"){
    assert(toList(filter((x:Int) => (x%2 == 0), fromList(List(1,2,3,4)))) == List(2,4))
    assert(toList(filter((x:Int) => (x%2 == 0), fromList(List(1,2,3)))) == List(2))
  }
  test("filter test empty"){
    assert(toList(filter((x:Any) => (x == 0), fromList(List()))) == List())
  }
  test("nth test 1"){
    assert(nth(fromList(List(1,2,3,4)), 3) == Some(4))
  }
  test("nth test 2"){
    assert(nth(fromList(List(1)), 0) == Some(1))
  }
  test("nth test 3"){
    assert(nth(fromList(List()), 0) == None)
  }
}