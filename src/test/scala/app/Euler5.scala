package app

/**
 * User: Eugene Dubrovka
 * Date: 9/25/11
 * Time: 12:16 AM
 */

object Euler5 extends App
{
	println((1 to 20).zip(List(1, 2, 3, 2, 5, 1, 7, 2, 3, 1, 11, 1, 13, 1, 1, 2, 17, 1, 19, 1)).foldLeft(1)( (r, v) => r * v._2))
}