package app

import helpers.primes.PrimesHelper
import annotation.tailrec

/**
 * User: Eugene Dubrovka
 * Date: 11/4/11
 * Time: 4:46 PM
 */

object Euler35 extends App
{
	val N = 1000000

	val primes = PrimesHelper.readPrimes(N)

	val circulars = scala.collection.mutable.Set[Int]()

	val primesSet = primes.toSet

	val powers = Array(1, 1, 10, 100, 1000, 10000, 100000) // powers are 10^(n-1)

	def isCircular(n: Int): Boolean =
	{
		@tailrec
		def check(n: Int, steps: Int, power: Int): Boolean =
		{
			if (primesSet.contains(n) && steps > 0)
			{
				val next = n / 10 + n % 10 * power
				check(next, steps - 1, power)
			}
			else steps == 0
		}

		val digits = math.log10(n).toInt + 1

		check(n, digits, powers(digits))
	}

	val result = primes.withFilter(_ < N).withFilter(isCircular _).map(x => x).size

	println(result)
}