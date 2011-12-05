package app

/**
 * User: Eugene Dubrovka
 * Date: 12/5/11
 * Time: 9:06 AM
 */

object Euler1 extends App
{
	val N = 1000

	def S(d: Int) = {
		val count = (N - 1) / d
		(1 + count) * count / 2 * d
	}

	println(S(3) + S(5) - S(15))

	println((1 until 1000).filter(x => x % 3 == 0 || x % 5 == 0).sum)
}