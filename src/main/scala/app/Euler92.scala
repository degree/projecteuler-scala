package app

import annotation.tailrec

/**
 * @author eugene.dubrovka
 * Date: 11/2/11
 * Time: 5:18 AM
 */

object Euler92 extends App
{
	val N = 10000000

	val LIMIT = (9 * 9 * math.log10(N) + 1).toInt

	val A = 1
	val B = 89

	var counter = 0

	val currentThread = scala.collection.mutable.Set[Int]()

	val as = scala.collection.mutable.Set[Int](A)
	val bs = scala.collection.mutable.Set[Int](B)

	def sum(n: Int): Int = {
		@tailrec
		def sumSquares(n: Int, s: Int): Int = {
			if (n > 0) {
				val digit = n % 10
				sumSquares(n / 10, s + digit * digit)
			}
			else s
		}

		sumSquares (n, 0)
	}

	@tailrec
	def endsWithOne(i: Int)(f: Int => Int = sum _): Boolean =
	{
		if (as.contains(i))
		{
			as ++= currentThread
			currentThread.clear()
			true
		}
		else if (bs.contains(i)) {
			bs ++= currentThread
			currentThread.clear()
			false
		}
		else
		{
			if (i < LIMIT) currentThread += i
			val next = f(i)
			if (next == A)
			{
				as ++= currentThread
				currentThread.clear()
				true
			}
			else if (next == B) {
				bs ++= currentThread
				currentThread.clear()
				false
			}
			else
			{
				endsWithOne(next)(f)
			}
		}
	}

	(1 until N) foreach { x => counter += (if (endsWithOne(x)(sum _)) 1 else 0) }

	println("End in 89: " + (N - counter - 1))
}