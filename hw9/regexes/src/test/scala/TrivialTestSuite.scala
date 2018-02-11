
class TrivialTestSuite extends org.scalatest.FunSuite {
	import Regexes._

	test("The Regexes object must be defined") {
		val regexes : hw.regex.RegexLike = Regexes
	}
	test("notAlphanumeric test"){
		val str = "abc"
		assert(notAlphanumeric.pattern.matcher(str).matches() == false)
	}
	test("notAlphanumeric test 2"){
		val str = "!@(!)*$!@($)*)!@$"
		assert(notAlphanumeric.pattern.matcher(str).matches() == true)
	}

	test("time test"){
		val str = "12:30"
		assert(time.pattern.matcher(str).matches() == true)
	}
	test("time test 2"){
		val str = "25:30"
		assert(time.pattern.matcher(str).matches() == false)
	}
	test("phone number test"){
		val num = "(123) 456-7890"
		assert(phone.pattern.matcher(num).matches() == true)

	}
	test("phone number test 2"){
		val num = "(123)456-7890"
		assert(phone.pattern.matcher(num).matches() == false)
	}
	test("zip test"){
		val str = "12345"
		assert(zip.pattern.matcher(str).matches == true)
	}
	test("zip test 2"){
		val str = "12345-6789"
		assert(zip.pattern.matcher(str).matches == true)
	}
	test("zip test 3"){
		val str = "123456"
		assert(zip.pattern.matcher(str).matches == false)
	}
	test("comment test"){
		val str = "/*hello */"
		assert(comment.pattern.matcher(str).matches == true)
	}
	test("comment test 2"){
		val str = "hello"
		assert(comment.pattern.matcher(str).matches == false)
	}
	test("numberPhrase test"){
		val str = "thirty-one"
		val str2= "ninety"
		assert(numberPhrase.pattern.matcher(str).matches == true)
		assert(numberPhrase.pattern.matcher(str2).matches == true)
	}
	test("roman test"){
		val str = "VI"
		assert(roman.pattern.matcher(str).matches == true)
	}
	test("date test"){
		val pass = "1990-08-30"
		val pass2 = "1990-09-30"
		val pass3 = "1996-02-29"
		val fail = "1990-02-29"
		val pass4 = "2017-02-28"
		assert(date.pattern.matcher(pass).matches == true)
		assert(date.pattern.matcher(pass2).matches == true)
		assert(date.pattern.matcher(pass3).matches == true)
		assert(date.pattern.matcher(pass4).matches == true)
		assert(date.pattern.matcher(fail).matches == false)
	}
	test("evenParity test"){
		val pass = "1032507"
		val fail = "20114423"
		val pass2 = "0002"
		assert(evenParity.pattern.matcher(pass).matches == true)
		assert(evenParity.pattern.matcher(pass2).matches == true)
		assert(evenParity.pattern.matcher(fail).matches == false)
	}

}
