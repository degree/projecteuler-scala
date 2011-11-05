package app

import helpers.primes.PrimesHelper

/**
 * User: Eugene Dubrovka
 * Date: 11/5/11
 * Time: 5:16 AM
 */

object Euler46 extends App
{
	val LIMIT = 10000 // I know the answer is 5777 ;)

	// list of primes for look-up
	val primes = PrimesHelper.readPrimes(LIMIT)

	// primes as a set
	val primesSet = primes.toSet

	// generate some squares for fast access without rounding
	val squares = (1 to math.sqrt(LIMIT).toInt).map(n => n * n).toSet

	// only odd composits
	val composits = (3 to LIMIT by 2).filterNot(n => primesSet.contains(n))

	// check if composite number cannot be represented as (prime + 2 * x * x)
	// we check only primes that are less than n
	// calculate a candidate for square
	// we need just one combination to get true
	def check(n: Int) = primes.withFilter(_ < n).map(x => (n - x) / 2).find(squares.contains) == None

	println(
		composits find check match
		{
			case Some(x) => x
			case _ => "None"
		}
	)
}