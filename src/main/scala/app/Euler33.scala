package app

import helpers.rational.Rational

/**
 * User: Eugene Dubrovka
 * Date: 11/4/11
 * Time: 1:06 PM
 */

object Euler33 extends App
{
	val ratios = for (
		middle <- 1 to 9;
		front <- 1 to 9;
		back <- 1 to 9;
		if back != middle;
		val ratio = Rational(front * 10 + middle, middle * 10 + back);
		val cutRatio = Rational(front, back);
		if ratio.compare(cutRatio) == 0
	) yield ratio

	println(((1: Rational) /: ratios) {_ * _} d) // where d is a field for denominator
}
