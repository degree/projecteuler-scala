package app

import helpers.primes.PrimesHelper

/**
 * User: Eugene Dubrovka
 * Date: 11/5/11
 * Time: 5:25 PM
 */

object Euler49 extends App
{
	val primes = PrimesHelper.readPrimes(10000).dropWhile(_ < 1000).filter(_ < 10000)

	val grouped = primes.groupBy(_.toString.sorted)

	def calculate(m: (String, List[Int])) =
	{
		m._2 match
		{
			case l @ List(_, _, _, _*) =>
			{
				val len = l.length

				for (
					a <- 0 until (len - 2);
					b <- (a + 1) until (len - 1);
					c <- (b + 1) until len;
					val la = l(a);
					val lb = l(b);
					val lc = l(c);
					if l(c) - l(b) == l(b) - l(a)
				) yield la.toString + lb.toString + lc.toString
			}
			case _ => List()
		}
	}

	println(grouped.flatMap(calculate _))
}