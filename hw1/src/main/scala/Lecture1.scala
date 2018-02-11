object Lecture1 {
	val oddNumbers = 1 :: 3 :: 5 :: Nil

	def sumDouble(lst: List[Int]): Int = lst match {
		case Nil => 0
		case n :: rest => n * 2 + sumDouble(rest) 
	}

	def removeZeroes(lst: List[Int]): List[Int] = lst match{
		case Nil => Nil
		case n :: rest if (n == 0) => removeZeroes(rest)
		case n :: rest => n :: removeZeroes(rest)

	}
	def countEvens(lst: List[Int]): Int = lst match {
		case Nil => 0
		case head :: tail if(head%2 == 0) => 1 + countEvens(tail)
		case head :: tail => countEvens(tail)
	}
	def removeAlternating(lst: List[String]): List[String] = lst match{
		case Nil => Nil
		case n :: (x :: rest) => n :: removeAlternating(rest)
		case _ => Nil
	}
	def isAscending(lst: List[Int]):  Boolean = lst match{
		case n :: Nil => true
		case n :: (x :: rest) if(x < n) => false
		case n :: (x :: rest) => isAscending(x :: rest)
	}
	def addSub(lst: List[Int]): Int = lst match{
		case Nil => 0
		case n :: x :: rest => n - x + addSub(rest)
		case n :: Nil => n 
	}
	def alternate(lst: List[Int], lst2: List[Int]): List[Int] = (lst, lst2) match{
		case (n :: rest, x :: rest2) => n :: x :: alternate(rest, rest2)
		case(n, Nil) => n
		case(Nil, n) => n 
	}
	def fromTo(x: Int, y: Int): List[Int] = {
		if(x>=y){
			Nil
		} else {
			x :: fromTo(x+1,y)
		}
	}
	def insertOrdered(n: Int, lst: List[Int]): List[Int] = (n, lst) match{
		case(n, x :: rest) if(n <= x) => n :: x :: rest
		case(n, x :: rest) => x :: insertOrdered(n,rest)
		case(n, Nil) => n :: Nil
	}
	def sort(lst: List[Int]): List[Int] = lst match{
		case(n :: x :: rest) => insertOrdered(n, sort(x :: rest))
		case(n :: Nil) => n :: Nil
		case(Nil) => Nil
	}
}