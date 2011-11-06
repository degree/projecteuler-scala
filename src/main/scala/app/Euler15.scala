package app

/**
 * User: Eugene Dubrovka
 * Date: 11/6/11
 * Time: 8:36 AM
 */

object Euler15 extends App
{
	// simple recursive factorial
	def fact(n: BigInt): BigInt = if (n > 1) n * fact(n - 1) else 1

	val f20 = fact(20)
	println(fact(40) / (f20 * f20))
}