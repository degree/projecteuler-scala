package app

import helpers.primes.PrimesHelper

/**
 * User: Eugene Dubrovka
 * Date: 11/6/11
 * Time: 1:37 PM
 */

object Euler21 extends App
{
	val N = 10000
	val primes = PrimesHelper.readPrimes(N)

	def d(n: Int) =
	{
		if (n > 1)
		{
			val ps = primes.withFilter(_ <= n / 2).map {
				p =>
					var N = n
					var power = 0
					while (N % p == 0)
					{
						N = N / p
						power += 1
					}
					(p, power)
			}.filter(_._2 > 0)
			(1 /: ps) { (p, t) => p * (math.pow(t._1, t._2 + 1).toInt - 1) / (t._1 - 1) } - n
		}
		else 0
	}

	def isAmicable(n: Int) =
	{
		val a = d(n)
		a != n && n == d(a)
	}

	val result = (2 until N).filter(isAmicable _).sum
	println(result)
}