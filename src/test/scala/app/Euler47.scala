package app

import helpers.primes.PrimesHelper

/**
 * User: Eugene Dubrovka
 * Date: 11/5/11
 * Time: 7:09 AM
 */

object Euler47 extends App
{
	val LIMIT = 100000
	val FACTORS = 4

	print("loading primes...")
	val primes = PrimesHelper.readPrimes(LIMIT)
	println(" [ Done ]")

	val start = (1 /: primes.take(FACTORS)) { _ * _ }

	def hasFactors(n: Int) =
	{
		var N = n
		val bound = N / 2
		primes.withFilter(_ <= bound).withFilter(N % _ == 0).map { p => while (N % p == 0) { N = N / p }; p }.size == FACTORS
	}

	val pairs: Stream[(Int, Boolean, Int)] =
	{
		def next(value: Int, idx: Int): Stream[(Int, Boolean, Int)] =
		{
			Stream.cons((value, hasFactors(value), idx), next(value + 1, idx + 1))
		}

		next(start, 0)
	}

	val is = 0 until FACTORS

	def check(t: (Int, Boolean, Int)) =
	{
		val index = t._3

		is.forall(i => pairs(index + i)._2)
	}

	println(
		pairs.find(check _) match {
			case Some((num, _, _)) => num
			case _ => 0
		}
	)
}