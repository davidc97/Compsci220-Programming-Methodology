class Game(turn: Player, dim: Int, gameboard: Matrix[Option[Player]]) extends GameLike[Game] {

  def isFinished(): Boolean = {
  	if(checkRows(gameboard.rows()) || checkCols(gameboard.cols()) || checkDiagonal(gameboard.mainDiagonal()) || checkDiagonal(gameboard.antiDiagonal) || isFull(gameboard.toMap()) ) {
  		true
  	} else{
  		false
  	}
  }
  def fullAndNoWinner(): Boolean = {
  	if(!checkRows(gameboard.rows()) && !checkCols(gameboard.cols()) && !checkDiagonal(gameboard.mainDiagonal()) && !checkDiagonal(gameboard.antiDiagonal) && isFull(gameboard.toMap()) ) {
  		true
  	} else{
  		false
  	}
  }
  def getBoard(): Matrix[Option[Player]] = {
  	gameboard
  }
  def isFull(map: Map[(Int,Int), Option[Player]]): Boolean = {
  	map.values.toList.exists(x => x == None)
  }
  def checkRows(rows: List[List[Option[Player]]]): Boolean = rows match{
  	case a :: b => if(check(a, Some(X)) || check(a, Some(O))){
  		true
  	} else {
  		checkRows(b)
  	}
  	case _ => false
  }
  def checkCols(cols: List[List[Option[Player]]]): Boolean = cols match{
  	case a :: b => if(check(a, Some(X)) || check(a, Some(O))){
  		true
  	} else {
  		checkCols(b)
  	}
  	case _ => false
  }
  def getTurn(): Option[Player] = {
  	Some(turn)
  }
  def checkDiagonal(diag: List[Option[Player]]): Boolean = {
  	if(check(diag, Some(X)) || check(diag, Some(O))) {
  		true
  	} else {
  		false
  	}
  }

  def check(lst: List[Option[Player]], prev: Option[Player]): Boolean = lst match{
  	case a :: b => if(prev == a){
  		check(b, a)
  	} else {
  		false
  	}
  	case _ => true
  }

  /* Assume that isFinished is true */
  def getWinner(): Option[Player] = {
  	if (rowWinner(gameboard.rows()) != None){
  		rowWinner(gameboard.rows())
  	} else if(colsWinner(gameboard.cols()) != None){
  		colsWinner(gameboard.cols())
  	} else if(diagonalWinner(gameboard.mainDiagonal()) != None){
  		diagonalWinner(gameboard.mainDiagonal())
  	} else {
  		diagonalWinner(gameboard.antiDiagonal())
  	}
  }

  def rowWinner(rows: List[List[Option[Player]]]): Option[Player] = rows match{
  	case a :: b => if(check(a, Some(O))){
  		Some(O)
  	} else if(check(a, Some(X))){
  		Some(X)
  	} else {
  		rowWinner(b)
  	}
  	case _ => None
  }
  def colsWinner(cols: List[List[Option[Player]]]): Option[Player] = cols match{
  	case a :: b => if(check(a, Some(O))){
  		Some(O)
  	} else if(check(a, Some(X))){
  		Some(X)
  	} else {
  		colsWinner(b)
  	}
  	case _ => None
  }
  def diagonalWinner(diag: List[Option[Player]]): Option[Player] = {
  	if(check(diag, Some(X))){
  		Some(X)
  	} else if(check(diag, Some(O))){
  		Some(O)
  	} else{
  		None
  	}
  }
  def getEmptySpaces(matrix: Matrix[Option[Player]], x:Int, y: Int): List[(Int,Int)] = {
  	if(x < dim && y < dim){
  		if(matrix.get(x,y) == None){
  			List((x,y)) ::: getEmptySpaces(matrix,x+1,y) ::: getEmptySpaces(matrix, x, y+1)
  		} else {
  			getEmptySpaces(matrix,x+1,y) ::: getEmptySpaces(matrix, x, y+1)
  		}
  	} else {
  		Nil
  	}
}
  
  def nextBoardsHelper(empty: List[(Int,Int)]): List[Game] = empty match{
  	case a :: b if (turn == O) => new Game(X, dim, gameboard.set(a._1,a._2,Some(turn))) :: nextBoardsHelper(b)
  	case a :: b if (turn == X) => new Game(O, dim, gameboard.set(a._1,a._2,Some(turn))) :: nextBoardsHelper(b)
  	case Nil => Nil
  }
  def nextBoards(): List[Game] = {
  	nextBoardsHelper(getEmptySpaces(gameboard, 0,0).distinct)
  }
}
object Solution extends MinimaxLike {

  type T = Game // T is an "abstract type member" of MinimaxLike
  def createGame(turn: Player, dim: Int, board: Map[(Int, Int), Player]): Game = {
  	val newboard = board map{case(a,b) => a -> Option(b)}
  	new Game(turn,dim, Matrix.fromMap(dim,None,newboard))
  }

  def minimax(board: Game): Option[Player] = {
  	if(board.getTurn() == Some(X)){
  		if(board.getWinner() == Some(X)){
  			Some(X)
  		} else if(board.fullAndNoWinner()){
  			None
  		} else {
  			if(board.nextBoards().map(nextboard => minimax(nextboard)).exists(x => x == Some(X))){
  				Some(X)
  			} else if(board.nextBoards().map(nextboard => minimax(nextboard)).exists(x => x == None)){
  				None
  			} else{
  				Some(O)
  			}
  		}
  	} else {
  		if(board.getWinner() == Some(O)){
  			Some(O)
  		} else if(board.fullAndNoWinner()){
  			None
  		} else {
  			if(board.nextBoards().map(nextboard => minimax(nextboard)).exists(x => x == Some(O))){
  				Some(O)
  			} else if(board.nextBoards().map(nextboard => minimax(nextboard)).exists(x => x == None)){
  				None
  			} else{
  				Some(X)
  			}
  		}
  	}
  }

}
