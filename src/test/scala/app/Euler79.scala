package app

/**
 * User: Eugene Dubrovka
 * Date: 11/5/11
 * Time: 7:19 PM
 */

object Euler79 extends App
{
	val checkCodesStr = "319\n680\n180\n690\n129\n620\n762\n689\n762\n318\n368\n710\n720\n710\n629\n168\n160\n689\n716\n731\n736\n729\n316\n729\n729\n710\n769\n290\n719\n680\n318\n389\n162\n289\n162\n718\n729\n319\n790\n680\n890\n362\n319\n760\n316\n729\n380\n319\n728\n716".split("\\s")

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
	var found = false;
	while (!found)
	{
		i += 1;

		found = check(i.toString)
	}

	println(i)
}