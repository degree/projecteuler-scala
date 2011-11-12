package app

import io.Source

/**
 * User: Eugene Dubrovka
 * Date: 11/11/11
 * Time: 3:10 PM
 */

// NOTE: this is not a pretty solution :(
object Euler81 extends App
{
	val filename = "/task_81.txt"

	val lines = Source.fromInputStream(getClass.getResourceAsStream(filename)).getLines().map(_.trim.split(",").map(_.toInt)).toArray

	// we know that every line is of length 'size'
	val size = lines(0).length

	val height = 2 * size - 1

	val matrix = Array.fill(height)(Array.fill(size)(1000000))

	for (row <- 0 until size; r <- row to 0 by -1; val c = row - r)
	{
		matrix(row)(c) = lines(r)(c)
	}

	for (row <- size until height; val cells = height - row; c <- 0 until cells; val r = size - c - 1)
	{
		matrix(row)(c) = lines(r)(row - r)
	}

	for (r <- (height - 1) to size by -1; c <- 0 to (height - r))
	{
		matrix(r - 1)(c) += math.min(matrix(r)(c), matrix(r)(math.max(0, c - 1)))
	}

	for (r <- (size - 2) to 0 by -1; c <- 0 to r)
	{
		matrix(r)(c) += math.min(matrix(r + 1)(c), matrix(r + 1)(math.min(c + 1, size - 1)))
	}

	println(matrix(0)(0))
}