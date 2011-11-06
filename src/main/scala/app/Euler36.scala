package app

/**
 * @author eugene.dubrovka
 * Date: 11/3/11
 * Time: 1:49 AM
 */

object Euler36 extends App
{
	def checkNumber(toD: => String, toB: => String) = toD.reverse == toD && toB.reverse == toB

	val result = (1 to 1000000).filter(n => checkNumber(n.toString, n.toBinaryString)).sum

	println(result)
//	println((1 to 1000000).withFilter(n => checkNumber(n.toString, n.toBinaryString)).map(x => x).sum.toString)
}