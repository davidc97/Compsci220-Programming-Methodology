import hw.parsing._
import scala.util.parsing.combinator._
object ArithEval extends ArithEvalLike {
def eval (e: Expr): Double = e match{
	case Num(x) => x
	case Add(x,y) => eval(x) + eval(y)
	case Sub(x,y) => eval(x) - eval(y)
	case Mul(x,y) => eval(x)*eval(y)
	case Div(x,y) => eval(x)/eval(y)
	case Exponent(x,y) => Math.pow(eval(x),eval(y))
}
}
object ArithParser extends ArithParserLike {
// number : PackratParser [Double] is defined in ArithParserLike
lazy val atom: PackratParser[Expr] = number ^^ (x => Num(x)) | "("~> expr <~")"
lazy val exponent: PackratParser[Expr] = exponent~("^"~>atom) ^^ {case x~y => Exponent(x,y)} | atom
lazy val add: PackratParser[Expr] = mul~("+"~>add) ^^ {case x~y => Add(x,y)} | mul~("-" ~> add) ^^ {case x~y => Sub(x,y)} | mul
lazy val mul: PackratParser[Expr] = exponent~("*"~> mul) ^^ {case x~y => Mul(x,y)} | exponent~("/" ~> mul) ^^ {case x~y => Div(x,y)} | exponent
lazy val expr: PackratParser[Expr] = add
}
object ArithPrinter extends ArithPrinterLike {
def print(e: Expr): String = e match{
	case Num(x) => x.toString
	case Add(x,y) => "(" + print(x) + "+" + print(y) + ")"
	case Sub(x,y) => "(" + print(x) + "-" + print(y) + ")"
	case Mul(x,y) => "(" + print(x) + "*" + print(y) + ")"
	case Div(x,y) => "(" + print(x) + "/" + print(y) + ")"
	case Exponent(x,y) => "(" + print(x) + "^" + print(y) + ")"
}
}