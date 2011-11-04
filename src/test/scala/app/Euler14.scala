package app

import annotation.tailrec
import scala.collection.mutable._

/**
 * @author eugene.dubrovka
 * Date: 11/3/11
 * Time: 5:16 AM
 */

object Euler14 extends App
{
	type ValueType = BigInt
	type LengthType = Int

	@inline
	def next(n: ValueType): ValueType = if (n % 2 == 0) n / 2 else 3 * n + 1

	val lengths = Map[ValueType, LengthType]((1: ValueType) -> (1: LengthType))

	def calcLength(n: ValueType)
	{
		@tailrec
		def lengthInternal(n: ValueType, len: LengthType): LengthType =
		{
			if (n == 1) len
			else {
				val nxt = next(n)
				if (lengths.contains(nxt)) lengths.get(nxt).get + len
				else lengthInternal(nxt, len + 1)
			}
		}

		lengths += n -> lengthInternal(n, 1)
	}

	val N = 1000000

	((1: ValueType) to N) foreach calcLength

	val idx = lengths.filter(_._1 < N).maxBy(_._2)._1

	println(idx)
}