package app

import annotation.tailrec

/**
 * User: Eugene Dubrovka
 * Date: 11/7/11
 * Time: 5:09 AM
 */

object Euler29 extends App
{
	type T = BigInt

	def power(a: T, b: Int) = {
		@tailrec
		def powerI(a: T, b: Int, p: T): T =
		{
			if (b > 0) powerI(a, b - 1, p * a) else p
		}

		powerI(a, b, 1)
	}

	println((for (a <- 2 to 100; b <- 2 to 100) yield power(a, b)).toSet.size)
}