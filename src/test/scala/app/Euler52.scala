package app

/**
 * User: Eugene Dubrovka
 * Date: 11/5/11
 * Time: 6:46 PM
 */

object Euler52 extends App
{
	def ns: Stream[Int] =
	{
		def next(n: Int): Stream[Int] = Stream.cons(n, next(n + 1))

		next(1)
	}


	@inline
	def t(n: Int, k: Int) = (n * k).toString.sorted

	println(ns find { n => (1 to 6).groupBy(t(n, _)).size == 1 })
}