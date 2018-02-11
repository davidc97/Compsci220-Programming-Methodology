import java.time.LocalDate
object DateImplicits{
	abstract class DateType
	case class Day(day: Int) extends DateType
	case class Month(month: Int) extends DateType
	case class Year(year: Int) extends DateType

	implicit class RichInt(int: Int){
		def jan(): LocalDate = LocalDate.of(LocalDate.now().getYear(),1, int) 
		def feb(): LocalDate = LocalDate.of(LocalDate.now().getYear(),2, int) 
		def mar(): LocalDate = LocalDate.of(LocalDate.now().getYear(),3, int) 
		def apr(): LocalDate = LocalDate.of(LocalDate.now().getYear(),4, int)
		def may(): LocalDate = LocalDate.of(LocalDate.now().getYear(),5, int)  
		def jun(): LocalDate = LocalDate.of(LocalDate.now().getYear(),6, int) 
		def jul(): LocalDate = LocalDate.of(LocalDate.now().getYear(),7, int) 
		def aug(): LocalDate = LocalDate.of(LocalDate.now().getYear(),8, int) 
		def sep(): LocalDate = LocalDate.of(LocalDate.now().getYear(),9, int) 
		def oct(): LocalDate = LocalDate.of(LocalDate.now().getYear(),10, int) 
		def nov(): LocalDate = LocalDate.of(LocalDate.now().getYear(),11, int) 
		def dec(): LocalDate = LocalDate.of(LocalDate.now().getYear(),12, int)
		def jan(year: Int): LocalDate = LocalDate.of(year,1, int)
		def feb(year: Int): LocalDate = LocalDate.of(year,2, int)
		def mar(year: Int): LocalDate = LocalDate.of(year,3, int)
		def apr(year: Int): LocalDate = LocalDate.of(year,4, int)
		def may(year: Int): LocalDate = LocalDate.of(year,5, int)
		def jun(year: Int): LocalDate = LocalDate.of(year,6, int)
		def jul(year: Int): LocalDate = LocalDate.of(year,7, int)
		def aug(year: Int): LocalDate = LocalDate.of(year,8, int)
		def sep(year: Int): LocalDate = LocalDate.of(year,9, int)
		def oct(year: Int): LocalDate = LocalDate.of(year,10, int)
		def nov(year: Int): LocalDate = LocalDate.of(year,11, int)
		def dec(year: Int): LocalDate = LocalDate.of(year,12, int)
		def days(): Day = new Day(int)
		def months(): Month = new Month(int)
		def years(): Year = new Year(int)
	}
	implicit class RichDate(date: LocalDate){
		def +(add: DateType): LocalDate = add match{
			case Day(x) => date.plusDays(x)
			case Month(x) => date.plusMonths(x)
			case Year(x) => date.plusYears(x)
		}
	}
}
