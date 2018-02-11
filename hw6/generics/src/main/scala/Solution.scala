import hw.generics._

sealed trait BinTree[A] extends ListLike[A, BinTree[A]] //recursive
case class Node[A](lhs: BinTree[A], value: A, rhs: BinTree[A]) extends BinTree[A]{
	def cons(head: A): BinTree[A] = Node(Leaf(), head, this)
	def head(): Option[A] = lhs.head() match{
		case Some(h) => Some(h)
		case None => Some(value)
	}
	def isEmpty(): Boolean = false
	def tail(): Option[BinTree[A]] = lhs.tail() match{
		case Some(t) => Some(Node(t,value,rhs))
		case None => Some(rhs)
	}

}
case class Leaf[A]() extends BinTree[A]{
	def cons(value: A): BinTree[A] = Node(Leaf(),value,Leaf())
	def head(): Option[A] = None
	def isEmpty(): Boolean = true
	def tail(): Option[BinTree[A]] = None

}

object ListFunctions {
	def filter[E, C <: ListLike[E, C]](f: E => Boolean, alist: C): C = {
  	listLikeMatch[E, C](alist) match {
    case None => alist
    case Some((hd, tl)) => {
      if (f(hd)) {
        filter(f, tl).cons(hd)
      } else {
        filter(f, tl)
      }
    }
  }
}

	def listLikeMatch[E, C <: ListLike[E, C]](alist: C): Option[(E, C)] = {
  (alist.head(), alist.tail()) match {
    case (Some(hd), Some(tl)) => Some((hd, tl))
    case (None, None) => None
    case _ => throw new IllegalArgumentException("Badly defined ListLike")
  }
}
	def append[E, C <: ListLike[E,C]](alist1: C, alist2: C): C = {
		listLikeMatch[E,C](alist1) match {
			case None => alist2
			case Some((hd, t1)) => append[E,C](alist2, t1).cons(hd)
		}
	}
	def insert[A <: hw.generics.Ordered[A], C <: hw.generics.ListLike[A,C]](elem: A, alist: C): C = {
		listLikeMatch[A,C](alist) match {
			case None => alist.cons(elem)
			case Some((hd, t1)) => if(elem.compare(hd) == LT || elem.compare(hd) == EQ){
				alist.cons(elem)
			} else {
				insert(elem, t1).cons(hd)
			}
		}
	}
	def sort[A <: hw.generics.Ordered[A], C <: hw.generics.ListLike[A,C]](alist: C): C = {
		listLikeMatch[A,C](alist) match{
			case Some((hd, t1)) => insert[A,C](hd, sort[A,C](t1))
			case None => alist
		}
	}
}
class C1 extends T2[Int,Int,String,String] with T3[Int,Int,Int,String,String,String,Int]{
// Do not change the class body . Simply extend T1 , T2 , and / or T3 .
def f(a: Int , b : Int ): Int = 0
def g(c: String ): String = ""
def h(d: String ): Int = 0
}
class C2 extends T1[Int,Int] with T2[Int,Int,Int,Int] with T3[Int,Int,Int,Int,Int,Int,Int]{
// Do not change the class body . Simply extend T1 , T2 , and / or T3 .
def f(a: Int , b : Int ): Int = 0
def g(c: Int ): Int = 0
def h(d: Int ): Int = 0
}
class C3[A](x: A) extends T3[Int,A,Int,A,String,String,A] {
// Do not change the class body . Simply extend T1 , T2 , and / or T3 .
def f(a: Int , b: A ): Int = 0
def g(c: A ): String = ""
def h(d: String ): A = x
}
class C4[A]( x : Int, y: C4[A]) extends T1[Int,C4[A]] with T3[Int,C4[A], C4[A], Int, C4[A], C4[A], Int]{
// Do not change the class body . Simply extend T1 , T2 , and / or T3 .
def f(a: Int , b : C4[A]): C4[A] = b
def g(c: Int ): C4[A] = y
def h(d: C4[A]): Int = x
}
