package app

import annotation.tailrec

/**
 * User: Eugene Dubrovka
 * Date: 11/11/11
 * Time: 01:15
 */

object Euler80 extends App
{
	val N = 100
	val digitsReverse = 9 to 0 by -1
	val rationals = (1 to 10) map {x => x * x}

	/**
	 * This formula will work only for integer numbers in range from 1 to 99.
	 * It is enough for this task
	 */
	def sumDigitsInRoot(N: Int)(n: Int) =
	{
		@tailrec
		def digitInternal(sum: Int, root: BigInt, rem: BigInt, count: Int, append: Int = 0): Int =
		{
			@inline
			def y(d: Int, r: BigInt) = ((20 * d) * r) + (d * d)

			if (count > 0)
			{
				val c = rem * 100 + append
				val d = (digitsReverse find {y(_, root) <= c}).get

				digitInternal(sum + d, root * 10 + d, c - y(d, root), count - 1)
			}
			else sum
		}

		digitInternal(0, 0, 0, N, n)
	}

	val sumNdigits = sumDigitsInRoot(N) _

	println(
		(0 /: (1 to 100)) {(s, n) => s + (if (!(rationals contains n)) sumNdigits(n) else 0)}
	)
}