object Homework2 {
	def map2[A,B,C](f: (A,B) => C, lst1: List[A], lst2: List[B]): List[C] = (lst1, lst2) match {
		case (_,Nil) => Nil
		case (Nil,_) => Nil
		case(h1 :: t1, h2 :: t2) => f(h1, h2) :: map2(f, t1,t2)
	}


	def zip[A,B](lst1: List[A], lst2: List[B]): List[(A,B)] = (lst1,lst2) match{
		case(_,Nil) => Nil
		case(Nil,_) => Nil
		case(h1 :: t1, h2 :: t2) => (h1,h2) :: zip(t1, t2)

	}

	def append[A](lst1: List[A], lst2: List[A]): List[A] = (lst1,lst2) match{
		case(h1 :: t1, h2 :: t2) => h1 :: append(t1, lst2)
		case(Nil, h2 :: t2) => h2 :: append(t2, Nil)
		case(h1 :: t1, Nil) => h1 :: append(t1, Nil)
		case(Nil, Nil) => Nil
	}
	def flatten[A](lst: List[List[A]]): List[A] = lst match{
		case h :: t => append(h, flatten(t))
		case Nil => Nil
	}

	def flatten3[A](lst: List[List[List[A]]]): List[A] = lst match{
		case h :: t => append(flatten(h),flatten3(t))
		case Nil => Nil
	}

	def append2[A](lst: List[A], x: A): List[A] = lst match{
		case h :: t => h :: append2(t, x)
		case Nil => x :: Nil
	}


	def buildList[A](length: Int, f: Int => A): List[A] = length match{
		case 0 => Nil
		case _ => append2(buildList(length-1,f),f(length-1))
	}

	def mapList[A, B](lst: List[A], f: A => List[B]): List[B] = lst match{
		case h :: t => append(f(h), mapList(t,f))
		case Nil => Nil
	}
	def partition[A](f: A => Boolean, lst: List[A]): (List[A], List[A]) = lst match{
		case h :: t if(f(h) == true) => (h :: partition(f,t)._1, partition(f,t)._2)
		case h :: t if(f(h) == false) => (partition(f,t)._1, h :: partition(f,t)._2)
		case Nil => (Nil,Nil)
	}
	def isEven(x: Int): Boolean = {x % 2 == 0}

}