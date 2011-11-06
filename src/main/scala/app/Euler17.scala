package app

/**
 * User: Eugene Dubrovka
 * Date: 11/6/11
 * Time: 8:52 AM
 */

object Euler17 extends App
{
	val numsStr = Map(
		0 -> "",
		1 -> "one",
		2 -> "two",
		3 -> "three",
		4 -> "four",
		5 -> "five",
		6 -> "six",
		7 -> "seven",
		8 -> "eight",
		9 -> "nine",
		10 -> "ten",
		11 -> "eleven",
		12 -> "twelve",
		13 -> "thirteen",
		14 -> "fourteen",
		15 -> "fifteen",
		16 -> "sixteen",
		17 -> "seventeen",
		18 -> "eighteen",
		19 -> "nineteen",
		20 -> "twenty",
		30 -> "thirty",
		40 -> "forty",
		50 -> "fifty",
		60 -> "sixty",
		70 -> "seventy",
		80 -> "eighty",
		90 -> "ninety",
		100 -> "hundred",
		1000 -> "thousand"
	)

	val mapping = numsStr map { t => (t._1, t._2.length())}

	def getLength(n: Int) =
	{
		val thousands = n / 1000
		val hundreds = (n % 1000) / 100
		val tens = (n % 100) / 10
		val ones = (n % 10)

		val tl = if (thousands > 0) mapping(thousands) + mapping(1000) else 0
		val hl = if (hundreds > 0) mapping(hundreds) + mapping(100) else 0
		val andl = if (hundreds > 0 && (tens > 0 || ones > 0)) 3 else 0
		val rest = if (tens == 1) mapping(n % 100) else (mapping(tens * 10) + mapping(ones))
		tl + hl + andl + rest
	}

	println((0 /: (1 to 1000)) { _ + getLength(_) })
}