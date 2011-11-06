package app

import helpers.primes.PrimesHelper

/**
 * User: Eugene Dubrovka
 * Date: 11/6/11
 * Time: 2:18 PM
 */

object Euler23 extends App
{
	val LIMIT = 28123
	val primes = PrimesHelper.readPrimes(LIMIT)

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

	def isAbudant(n: Int) = n < d(n)

	val abundantNumbers = (1 to LIMIT) filter isAbudant
	val ans = abundantNumbers.toSet

	def canBeSummed(n: Int) =
	{
		abundantNumbers.find(a => ans.contains(n - a)) match {
			case Some(_) => true
			case None => false
		}
	}

	val r = (0 /: (1 to LIMIT)) { (s, n) => s + (if (canBeSummed(n)) 0 else n)}

	println(r)
}