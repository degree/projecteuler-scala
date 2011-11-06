package app

/**
 * User: Eugene Dubrovka
 * Date: 9/24/11
 * Time: 11:32 PM
 */

object Euler2 extends App
{
	lazy val fib: Stream[Int] = Stream.cons(1, Stream.cons(2, fib.zip(fib.tail).map(p => p._1 + p._2)))

	val result = fib.filter( _ % 2 == 0 ).takeWhile( _ < 4000000).reduce(_ + _)

	println(result)
}