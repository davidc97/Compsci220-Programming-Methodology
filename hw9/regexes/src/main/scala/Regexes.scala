import scala.util.matching.Regex

object Regexes extends hw.regex.RegexLike{
def notAlphanumeric: Regex = "([^A-Za-z0-9])+".r
def time: Regex = """((2[0-3])|([0-1][0-9])):[0-5][0-9]""".r
def phone: Regex = "[(]\\d{3}[)]\\s\\d{3}[-]\\d{4}".r
def zip: Regex = "\\d{5}|\\d{5}[-]\\d{4}".r
def comment: Regex = """/\*.*\*/""".r
def numberPhrase: Regex = "(twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety)(?:-one|-two|-three|-four|-five|-six|-seven|-eight|-nine)?".r
def roman: Regex = "^M{0,4}(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$".r
def date: Regex = "([0-9][0-9])((([13579][26]|[02468][048])-(((02)-([0-2][0-9]))|((1[02]|0[13578])-([0-2][0-9]|3[01]))|((11|0[469])-([0-2][0-9]|30))))|(([0-9][0-9]))-(((1[02]|0[13578])-([0-2][0-9]|3[01]))|((02)-([0-2][0-8]))|((11|0[469])-([0-2][0-9]|30))))".r
def evenParity: Regex = "[^(1|3|5|7|9)]*((1|3|5|7|9)[^(1|3|5|7|9)]*(1|3|5|7|9)[^(1|3|5|7|9)]*)*".r
}
