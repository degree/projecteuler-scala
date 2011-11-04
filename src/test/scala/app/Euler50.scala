package app

import collection.mutable.ListBuffer
import helpers.primes.PrimesHelper

/**
 * User: Eugene Dubrovka
 * Date: 11/2/11
 * Time: 2:22 PM
 */

object Euler50 extends App
{
	val primes = PrimesHelper.readPrimes(100)

	val (buffer, sum) = ((new ListBuffer[Int](), 0) /: primes) { (t: (ListBuffer[Int], Int), item) => val sum = t._2 + item; t._1 += sum; (t._1, sum) }

	println(primes)
	println(buffer)

	println(primes.toSet.intersect(buffer.toSet).max)
}