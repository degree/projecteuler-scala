package app

import annotation.tailrec

/**
 * User: Eugene Dubrovka
 * Date: 11/12/11
 * Time: 10:32 AM
 */

object Euler40 extends App
{
	@tailrec
	def getNext(sb: StringBuilder, x: Int = 0): StringBuilder = if (sb.length < 1000001) getNext(sb.append(x), x + 1) else sb

	val sb = getNext(new StringBuilder)
	val digit = sb(_: Int).asDigit
	val power = (p: Int) => math.pow(10, p).toInt

	val result = (1 /: ((0 to 6) map power map digit)) {_ * _}

	println(result)
}