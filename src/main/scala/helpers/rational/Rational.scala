package helpers.rational

/**
 * User: Eugene Dubrovka <eugene.dubrovka@gmail.com>
 * Date: 03.07.11
 * Time: 0:20
 */

object Rational
{
	implicit def intToRational(i: Int) = new Rational(i)
}

case class Rational(numerator: Int, denominator: Int) extends Ordered[Rational]
{
	require(denominator != 0)

	// functions
	// greates common divisor
	private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

	private val g = gcd(numerator.abs, denominator.abs)

	val n = numerator / g
	val d = denominator / g

	// constructors
	def this(numerator: Int) = this (numerator, 1)

	override def toString = n + "/" + d

	// arithmetic
	def +(that: Rational) = new Rational(that.d * this.n + that.n * this.d, that.d * this.d)

	def +(n: Int) = new Rational(this.n + n * this.d, this.d)

	def -(that: Rational) = new Rational(that.d * this.n - that.n * this.d, that.d * this.d)

	def -(n: Int) = new Rational(this.n - n * this.d, this.d)

	def *(that: Rational) = new Rational(this.n * that.n, this.d * that.d)

	def *(n: Int) = new Rational(this.n * n, this.d)

	def /(that: Rational) = new Rational(this.n * that.d, this.d * that.n)

	def /(d: Int) = new Rational(this.n, this.d * d)

	def max(that: Rational) = if (this < that) this else that

	def compare(that: Rational) = (this.n * that.d) - (that.n * this.d)
}