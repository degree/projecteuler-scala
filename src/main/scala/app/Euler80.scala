package app

import annotation.tailrec

/**
 * User: Eugene Dubrovka
 * Date: 11/11/11
 * Time: 01:15
 */

object Euler80 extends App
{
	val NUM_DIGITS = 100
	val DIGITS = 9 to 0 by -1
	val RATIONAL_SQUARES = ((1 to 10) map {x => x * x}).toSet

	/**
	 * This formula will work only for integer numbers in range from 1 to 99.
	 * It is enough for this task
	 */
	def sumDigitsInRoot(numDigits: Int)(n: Int) =
	{
		@tailrec
		def calcNextDigit(sum: Int, root: BigInt, rem: BigInt, count: Int, append: Int = 0): Int =
		{
			@inline
			def y(d: Int, r: BigInt) = ((20 * d) * r) + (d * d)

			if (count > 0)
			{
				val c = rem * 100 + append
				val d = (DIGITS find {y(_, root) <= c}).get

				calcNextDigit(sum + d, root * 10 + d, c - y(d, root), count - 1)
			}
			else sum
		}

		calcNextDigit(0, 0, 0, numDigits, n)
	}

	val sumNdigits = sumDigitsInRoot(NUM_DIGITS) _
	val irrationalRootsOnly = (n: Int) => !(RATIONAL_SQUARES contains n)

	println(
		(1 to 100) withFilter irrationalRootsOnly map sumNdigits sum
	)
}