object FunctionalDataStructures {

  //
  // Part 1. Persistent Queues
  //

  def enqueue[A](elt: A, q: Queue[A]): Queue[A] = q match{
    case Queue(a,b) => Queue(a, elt :: b)
  }

  def dequeue[A](q: Queue[A]): Option[(A, Queue[A])] = q match{
    case Queue(List(),List()) => None
    case Queue(List(), b) => {
      val x = b.reverse.head
      Some(x,Queue(b.reverse.filter(y => y != x), List()))
    }
    case Queue(h :: t,b) => Some(h, Queue(t,b))
  }

  //
  // Part 2. Join Lists
  //

  def max[A](lst: JoinList[A], compare: (A, A) => Boolean): Option[A] = lst match{
    case Empty() => None
    case Singleton(a) => Some(a)
    case Join(Singleton(a),Singleton(b),c) => if(compare(a,b)){
      Some(a)
    } else {
      Some(b)
    }
    case Join(a,b,c) => if(compare(max(a,compare).get,max(b,compare).get)) {
      max(a,compare)
    } else {
      max(b,compare)
    }
  }

  def first[A](lst: JoinList[A]): Option[A] = lst match{
    case Empty() => None
    case Singleton(a) => Some(a)
    case Join(a,b,c) => if(a.size == 0){
      first(b)
    } else {
      first(a)
    }
  }

  def rest[A](lst: JoinList[A]): Option[JoinList[A]] = lst match{
    case Empty() => None
    case Singleton(a) => Some(Empty())
    case Join(a,b,c) => if(a.size == 0){
      Some(Join(a,filter((x:A)=> (Some(x) != first(b)), b),c-1))
      } else{
      Some(Join(filter((x:A) => (Some(x) != first(a)), a), b, c-1))
    }
  }

  def nth[A](lst: JoinList[A], n: Int): Option[A] = (lst,n) match {
    case (Empty(), x) => None
    case (_, 0) => first(lst)
    case(_,x) => nth(rest(lst).get, n-1) 
  }

  def map[A,B](f: A => B, lst: JoinList[A]): JoinList[B] = lst match{
    case Empty() => Empty()
    case Singleton(a) => Singleton(f(a))
    case Join(a,b,c) => Join(map(f,a), map(f,b), c)
  }

  def filter[A](pred: A => Boolean, lst: JoinList[A]): JoinList[A] = lst match{
    case Empty() => Empty()
    case Singleton(a) => if(pred(a)){
      Singleton(a)
    } else {
      Empty()
    }
    case Join(a,b,c) => {
      val x = filter(pred,a)
      val y = filter(pred,b)
      val z = x.size + y.size
      Join(x,y,z)
    }
  }

}