object Homework3 {

  import edu.umass.cs.CSV


  // WARNING: this may take a very long time. Cut the file or work with a
  // small, made-up dataset if you have trouble.
   val allBirths = CSV.fromFile("ssa-births.csv")

  val lifeExpectancy = CSV.fromFile("cdc-life-expectancy.csv")

  def contains(lst: List[String], n: Int): Boolean = lst match{
  	case h :: t => if(h == n.toString()) {
  		true
  		} else {
  			contains(t, n)
  		}
  	case Nil => false
  }
  /** Restrict the data to the year `year`. */
  def yearIs(data: List[List[String]], n: Int): List[List[String]] = {
  	data.filter(x => Integer.parseInt(extractFirst(x)) == n)
  }

  def extractFirst(lst: List[String]): String = lst match{
  	case h :: t => h
  	case Nil => ""
  }

  def extractNest(lst: List[List[String]]): List[String] = lst match{
  	case h :: t => h
  	case Nil => List()
  }

  /** Restrict the data to years greater than `bound`. */
  def yearGT(data: List[List[String]], bound: Int): List[List[String]] = {
  	data.filter(x => Integer.parseInt(extractFirst(x)) > bound)
  } 

  /** Restrict the data to years less than `bound` */
  def yearLT(data: List[List[String]], bound: Int): List[List[String]] = {
  	data.filter(x => Integer.parseInt(extractFirst(x)) < bound)
  } 

  /** Restrict the data to the name `name`. */
  def onlyName(data: List[List[String]], name: String): List[List[String]] = data match{
  	case h :: t => if(extractFirst(h.drop(1)) == name){
  		h :: onlyName(t, name)
  	} else {
  		onlyName(t, name)
  	}
  	case Nil => Nil
  }
  def mostPopularHelper(data: List[List[String]], num: Int): Int = data match{
  	case h :: t => if(Integer.parseInt(extractFirst(h.drop(3))) >= num) {
  		mostPopularHelper(t, Integer.parseInt(extractFirst(h.drop(3))))
  	} else{
  		mostPopularHelper(t, num)
  	}
  	case Nil => num
  }
  /** Calculate the most popular name and the number of children born with
      that name. */
  def mostPopular(data: List[List[String]]): (String, Int) = data match{
  	case h :: t => if(Integer.parseInt(extractFirst(h.drop(3))) == mostPopularHelper(data, 0)){
  		(extractFirst(h.drop(1)), Integer.parseInt(extractFirst(h.drop(3))))
  	} else {
  		mostPopular(t)
  	}
  	case Nil => ("no data has been entered", 0)
  }

  /** Calculate the number of children born in the given dataset. */
  def count(data: List[List[String]]): Int = {
  	data.foldRight(0)((x, acc) => Integer.parseInt(extractFirst(x.drop(3))) + acc)
  }

  // def checkGender(data: List[String]): Boolean = {
  // 	if(extractFirst(data.drop(2)) == "F"){
  // 		return true
  // 	} else {
  // 		return false
  // 	}
  // }
  val isMale = (data: List[String]) => {
  	extractFirst(data.drop(2)) =="M"
  }
  val isFemale = (data: List[String]) => {
  	extractFirst(data.drop(2)) =="F"
  }
  /** Produce a tuple with the number of girls and boys respectively. */
  def countGirlsAndBoys(data: List[List[String]]): (Int, Int) = {
  	(count(data.filter(isFemale)),count(data.filter(isMale)))
  }

  def getName(data: List[String]): String = {
  	extractFirst(data.drop(1))
  }

  def gM(data: List[List[String]]): List[List[String]] = {
  data.filter(x=> isMale(x))
}
  def getMaleNames(data: List[List[String]]): Set[String] = {
    Set(gM(data).map(x => x.drop(1).take(1)))
  }
  def getFemaleNames(data: List[List[String]]): Set[String] = data match{
  	case h :: t => if(extractFirst(h.drop(2)) == "F"){
  		h.drop(1).take(1).toSet ++ getFemaleNames(t)
  		} else{
  			getFemaleNames(t)
  		}
  	case Nil => Set()
  }
  /** Calculate the set of names that are given to both girls and boys. */
  def unisexNames(data: List[List[String]]): Set[String] = {
 	getMaleNames(data).intersect(getFemaleNames(data))
  }


  /** Determine if a person with the specified `gender` (either "M" or "F") who
      was born in `birthYear` is expected to be alive, according to the CDC
      life-expectancy data.

      If `currentYear` is the last year the person is estimated to be alive, be
      optimistic and produce `true`.

      The CDC data only ranges from 1930 -- 2010. Therefore, assume that
      `birthYear` is in this range too. */

  def expectedAlive(gender: String, birthYear: Int, currentYear: Int): Boolean = gender match{
  	case "M" => if(Integer.parseInt(extractFirst(extractNest(yearIs(lifeExpectancy, birthYear)).drop(1))) < currentYear-birthYear){
  		false
  	} else {
  		true
  	}
  	case "F" => if(Integer.parseInt(extractFirst(extractNest(yearIs(lifeExpectancy, birthYear)).drop(2))) < currentYear-birthYear){
  		false
  	} else {
  		true
  	}
  	case _ => false
  }
  def epHelper(data: List[List[String]], year: Int): List[List[String]] = {
  	data.filter(x=> expectedAlive(extractFirst(x.drop(2)), Integer.parseInt(extractFirst(x)), year))
  } 
  /** Estimate how many people from `data` will be alive in `year`. */
  def estimatePopulation(data: List[List[String]], year: Int): Int = {
  // 	case h :: t => if(expectedAlive(extractFirst(h.drop(2)), Integer.parseInt(extractFirst(h)), year)){
  // 		1 + estimatePopulation(t, year)
  // 	} else {
  // 		estimatePopulation(t, year)
  // 	}
  // 	case Nil => 0
  // }
 epHelper(data, year).foldRight(0)((x,acc) => Integer.parseInt(extractFirst(x.drop(3))) + acc)
}
}