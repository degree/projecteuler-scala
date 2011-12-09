package app

/**
 * User: Eugene Dubrovka
 * Date: 11/6/11
 * Time: 4:15 PM
 */

object Euler19 extends App
{
	implicit def monthToFunc(n: Int) = (_: Int) => n

	val months = List[(Int) => Int](
		31, (year: Int) => if (year % 400 == 0 || (year % 100 != 0 && year % 4 == 0)) 29 else 28, 31, // jan, feb, mar
		30, 31, 30, // apr, may, jun
		31, 31, 30, // jul, aug, sep
		31, 30, 31 // oct, nov, dec
	)

	// a terrible one-liner
//	def count(y1: Int, y2: Int) = (1 :: (Range(y1, y2 + 1).map {y => months.map(f => (f(y) % 7))}.flatten.toList)).foldLeft((List[Int](), 0)) {(t, i) => {val x = t._2 + i; (x :: t._1, x)}}._1.reverse.map {_ % 7}.count {_ == 0}
	def count(y1: Int, y2: Int) =
	{
		val rangeYears = Range(y1, y2 + 1)
		val firstDaysOfMonthsInYearRange = 1 :: (rangeYears.flatMap(y => months.map(f => (f(y) % 7)))).toList
		val tuple = ((List[Int](), 0) /: firstDaysOfMonthsInYearRange) {(t, i) => {val x = t._2 + i; (x :: t._1, x)}}
		tuple._1.reverse.map(_ % 7).count(_ == 0)
	}

	println(count(1900, 2001) - count(1900, 1901))
}