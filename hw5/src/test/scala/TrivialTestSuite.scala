import Solution._

class TrivialTestSuite extends org.scalatest.FunSuite {
  test("The solution object must be defined") {
    val obj : MinimaxLike = Solution
  }
  test("nextBoards"){
  	assert(createGame(X,2,Map()).nextBoards().length == 4)
  }
}