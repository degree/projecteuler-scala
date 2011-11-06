package helpers.primes

/**
 * User: Eugene Dubrovka
 * Date: 11/2/11
 * Time: 2:21 PM
 */

object PrimesHelper
{
	def readPrimes(limit: Int) =
	{
		def splitNums(l: String) = l.trim.split("\\s+").map(_.toInt)
		val lines = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/src/main/resources/primes.txt")).getLines()

		lines.takeWhile(l => splitNums(l).min <= limit).foldLeft(List[Int]()) {
			(l, s) => l ++ splitNums(s).toList
		}
	}
}