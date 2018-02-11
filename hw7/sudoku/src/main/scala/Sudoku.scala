import hw.sudoku._

object Solution extends SudokuLike {
  type T = Board

  def parse(str: String): Board = {
    val emptyBoard = Map((0.to(8).flatMap{r=>0.to(8).map{c=>((r,c)->List(1,2,3,4,5,6,7,8,9))}}):_*)
    val strIndexed = str.toList.zipWithIndex
    parseHelper(makeEmptyBoard(),strIndexed)    
  }

  def parseHelper(board: Board, lst: List[(Char,Int)]): Board = lst match {
    case Nil => board
    case a :: b => if (a._1 == '.'){
      parseHelper(board,b)
    } else {
      val row = (a._2)/9
      val col = (a._2)%9
      val int = a._1.toInt-48
      parseHelper(board.place(row,col,int), b)
    }
  }

  // def parseHelper(board: Map[(Int,Int), List[Int]], lst: List[(Char, Int)]): Map[(Int,Int),List[Int]] = lst match {
  //   case Nil => board
  //   case a :: b => if(a._1 == '.'){
  //     parseHelper(board,b)
  //   } else {
  //     val row = (a._2)/9
  //     val col = (a._2)%9
  //     val int = List(a._1.toInt-48)
  //     parseHelper(removeFromPeers(peers(row,col), a._1.toInt-48, board), b) + ((row,col) -> int) 
  //   }
  // }
  def removeFromPeers(peers: List[(Int,Int)], value: Int, board: Map[(Int,Int), List[Int]]): Map[(Int,Int), List[Int]] = peers match {
    case Nil => board
    case a :: b => removeFromPeers(peers.tail,value,board) + (a -> board(a).filterNot(elm => elm == value))
  }

  def makeEmptyBoard(): Board = {
    new Board(Map((0.to(8).flatMap{r=>0.to(8).map{c=>((r,c)->List(1,2,3,4,5,6,7,8,9))}}):_*))
  }

  // You can use a Set instead of a List (or, any Iterable)
  def peers(row: Int, col: Int): List[(Int, Int)] = {
    peersTbl((row,col))
  }
  def calcPeers(row: Int, col: Int): List[(Int, Int)] = {
    val rowPeers = 0.to(8).map{r=>(r,col)}
    val colPeers = 0.to(8).map{c=>(row,c)}
    val boxRow = (row/3) * 3
    val boxCol = (col/3) * 3
    val boxPeers = boxRow.to(boxRow + 2).flatMap{r=>boxCol.to(boxCol+2).map{c=>(r,c)}}
    (rowPeers ++ colPeers ++ boxPeers).filterNot{
      case(r,c) => r== row&&col == c
    }.toList.distinct
  }
    val peersTbl = Map((0.to(8).flatMap{r=>0.to(8).map{c=>((r,c)->calcPeers(r,c))}}):_*)
}

// Top-left corner is (0,0). Bottom-right corner is (8,8). Feel free to
// change the fields of this class.
class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {

  def availableValuesAt(row: Int, col: Int): List[Int] = {
    // Assumes that a missing value means all values are available. Feel
    // free to change this.
    available.getOrElse((row, col), 1.to(9).toList)
  }

  def valueAt(row: Int, col: Int): Option[Int] = {
    if(available((row,col)).length == 1){
      Option(available((row,col)).head)
    } else {
      None
    }
  }

  def isSolved(): Boolean = {
    available.values.toList.filter(x => x.length == 1).length == 81
  }

  def isUnsolvable(): Boolean = {
    available.values.toList.filter(x=> x.isEmpty).length >= 1
  }

  def place(row: Int, col: Int, value: Int): Board = {
    require(availableValuesAt(row, col).contains(value))
    // new Board(placeHelper(Solution.peers(row,col),value,available) + ((row,col) -> List(value)))
    new Board(check(Solution.peers(row,col),placeHelper(Solution.peers(row,col),value,available) + ((row,col) -> List(value))))
  }

  def placeHelper(peersList: List[(Int,Int)], value: Int, board: Map[(Int,Int), List[Int]]): Map[(Int,Int), List[Int]] = peersList match {
    case Nil => board
    case a :: b => {
      val x = (a -> board(a).filterNot(elm => elm == value))
        placeHelper(peersList.tail,value,board) + x 
  }
}

  def check(peersList: List[(Int,Int)], board: Map[(Int,Int), List[Int]]): Map[(Int,Int), List[Int]] = peersList match {
    case Nil => board
    case a :: b => {
      if(board(a).length == 1){
        check(b,placeHelper(Solution.peers(a._1,a._2), board(a).head, board))
      } else {
        check(b,board)
      }
    }
  }

  // You can return any Iterable (e.g., Stream)
  def nextStates(): Stream[Board] = {
    if (isUnsolvable()) {
      // return 
      Stream()
    } else {
      nextStatesHelper(available, available.keys.toList).distinct 
    }
  }
  def nextStatesHelper(board: Map[(Int,Int), List[Int]], coord: List[(Int,Int)]): Stream[Board] = coord match{
    case Nil => Stream()
    case a :: b => if (board(a).isEmpty){
      nextStatesHelper(board, b)
    } else if(place(a._1,a._2,board(a._1,a._2).head).isUnsolvable){
      nextStatesHelper(board + ((a._1,a._2) -> board(a._1,a._2).tail), coord)
    } else {
      nextStatesHelper(board + ((a._1,a._2) -> board(a._1,a._2).tail), coord) #::: Stream(place(a._1,a._2,board(a._1,a._2).head))
    }
  }
  // def nextStatesHelper(board: Map[(Int,Int), List[Int]], coord: List[(Int,Int)]): Stream[Board] = coord match{
  //   case Nil => Stream()
  //   case a :: b => if (board(a).length == 0){
  //     nextStatesHelper(board, coord.tail)
  //   } else if (place(a._1,a._2,board(a._1,a._2).head).isUnsolvable){
  //     nextStatesHelper(board + ((a._1,a._2) -> board(a._1,a._2).filterNot(elm => elm == board(a._1,a._2).head)), coord)
  //   }
  //     else {
  //     nextStatesHelper(board + ((a._1,a._2) -> board(a._1,a._2).filterNot(elm => elm == board(a._1,a._2).head)), coord).append(Stream(place(a._1,a._2,board(a._1,a._2).head)))
  //     // nextStatesHelper(board + ((a._1._1,a._1._2) -> board(a._1._1,a._1._2).filterNot(elm => elm == a._2.head))).append(Stream(place(a._1._1,a._1._2,a._2.head)))
  //   }
  // }

  def solve(): Option[Board] = {
    // solveHelper(this.nextStates())
    if(isSolved()){
      Some(this)
    } else if(nextStates() == Stream()){
      None
    } else {
      val y = nextStates().map(x => x.solve()).filterNot(z => z == None)
      if(y.isEmpty){
        None
      } else {
        y.head
      }
      
    //   val y = nextStates().map(nextboard => if(nextboard != this) {nextboard.solve()} else {None}).filterNot(x => x == None)
    //   if(y.isEmpty){
    //     None
    //   } else {
    //     y.head
    //   }
    // }
  }
}
  def solveHelper(board: Stream[Board]): Option[Board] = board match{
    case a #:: b => if(a.isSolved()){
      Some(a)
    } else if(a.nextStates().length == 0){
      None
    } else {
      solveHelper(b)
    }
    case a #:: Stream() => solveHelper(a.nextStates)
  }
}