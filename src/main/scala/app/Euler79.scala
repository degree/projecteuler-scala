package app

import io.Source

/**
 * User: Eugene Dubrovka
 * Date: 11/5/11
 * Time: 7:19 PM
 */

object Euler79 extends App
{
	val checkCodesStr = Source.fromInputStream(getClass.getResourceAsStream("/src/main/resources/keylog.txt")).getLines().toList

	@inline
	def checkCode(code: String, check: String): Boolean = {
		val a = code.indexOf(check(0))
		val b = code.indexOf(check(1))
		val c = code.indexOf(check(2))
		(a > -1) && (b > -1) && (c > -1) && a < b && b < c
	}

	@inline
	def check(code: String) = checkCodesStr forall { checkCode (code, _) }

	var i: Int = 0;
	while (!check(i.toString)) i += 1

	println(i)
}