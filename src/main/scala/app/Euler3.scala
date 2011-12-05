package app

import helpers.primes.PrimesHelper

/**
 * User: Eugene Dubrovka
 * Date: 12/5/11
 * Time: 9:13 AM
 */

object Euler3 extends App
{
	val N: BigInt = 600851475143L

	val primes = PrimesHelper.readPrimes(10000);

	println(primes.filter(N % _ == 0).max)
}