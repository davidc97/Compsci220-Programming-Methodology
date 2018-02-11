// You may modify or delete this file
import Homework3._
class TestSuite extends org.scalatest.FunSuite {

  import edu.umass.cs.CSV

  val allBirths = CSV.fromFile("ssa-births.csv")
  val le = CSV.fromFile("cdc-life-expectancy.csv")
  val sb = CSV.fromFile("lessbirths.csv")
  test("Have life expectancies from 1930 -- 2010") {
    assert(le.map(x => x(0).toInt).reverse
           == 1930.to(2010))
  }
  test("yearIs test"){
  	assert(yearIs(le,2008) == List(List("2008","75","80")))
  }

  test("yearIs births"){
  	assert(yearIs(sb,2008) == List(List("2008","Emiah","F","11"),List("2008","Emireth","F","11"),List("2008","Emmory","F","11")))
  }
  test("yearGT test"){
  	assert(yearGT(le,2008) == List(List("2010", "76", "81"), List("2009","75","80")))
  }
  test("yearLT test"){
  	assert(yearLT(le,2011) == le)
  }
}