package app

import annotation.tailrec

/**
 * User: Eugene Dubrovka
 * Date: 11/10/11
 * Time: 4:09 PM
 */

object Euler80 extends App
{
	val N = 20

	def sumDigits(n: Int): Int =
	{
		@tailrec
		def digitInternal(sum: Int, p: BigInt, r: BigInt, count: Int, first: Boolean): Int =
		{
			if (count > 0)
			{
				val c: BigInt = r * 100 + (if (first) n else 0)

				var digit = -1
				var y: BigInt = 0
				do
				{
					digit += 1
					y = (20 * p + digit) * digit
				} while (y <= c)

				digit -= 1
				y = (20 * p + digit) * digit

				print(digit)
				digitInternal(sum + digit, p * 10 + digit, c - y, count - 1, false)
			}
			else
			{
				println()
				sum
			}
		}

		digitInternal(0, 0, 0, N, true)
	}

	println("141421356237309504880")
	println(sumDigits(2))
}