package app

import annotation.tailrec

/**
 * User: Eugene Dubrovka
 * Date: 11/7/11
 * Time: 5:21 AM
 */

object Euler30 extends App
{
	val POWER = 5
	val LIMIT = 6 * 59049

	def power(p: Int)(x: Int) = {
		@inline
		def next(p: Int, product: Int): Int =
		{
			if (p > 0) next(p - 1, product * x) else product
		}

		next(p, 1)
	}

	val powers = (0 to 9) map power(POWER)

	def sumPowers(n: Int) = {
		@tailrec
		def next(n: Int, sum: Int): Int =
		{
			if (n > 0) next(n / 10, sum + powers(n % 10)) else sum
		}

		next(n, 0)
	}

	@inline
	def isProper(n: Int) = n == sumPowers(n)

	val result = (0 /: (2 to LIMIT)) { (s, n) => s + (if (isProper(n)) n else 0) }

	println(result)
}