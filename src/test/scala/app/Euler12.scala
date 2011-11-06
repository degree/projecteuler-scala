package app

import helpers.primes.PrimesHelper

/**
 * User: Eugene Dubrovka
 * Date: 11/6/11
 * Time: 6:32 AM
 */

object Euler12 extends App
{
	val LIMIT = args(0).toInt
	val DIVISORS = args(1).toInt
	print("Loading primes...")
	val primes = PrimesHelper.readPrimes(LIMIT)
	println(" [ Done ]")

	type T = BigInt

	def triangles: Stream[T] =
	{
		def next(sum: T, idx: T): Stream[T] =
		{
			val value = sum + idx
			print(value)
			Stream.cons(value, next(value, idx + 1))
		}

		next(0, 1)
	}


	def countDivisors(n: T) = {
//		val maxPrime = n / 2

		val powers = primes.withFilter(_ <= n).map{
			p =>
				var N = n
				var k = 1
				while (N % p == 0)
				{
					N = N / p
					k += 1
				}
				k
		}
		val k = (1 /: powers) { _ * _ }
		println(":" + k)
		k
	}

	println (triangles find { n => countDivisors(n) > DIVISORS})
}