package app

/**
 * @author eugene.dubrovka
 * Date: 9/15/11
 * Time: 2:37 PM
 */

object Euler18 extends App
{
	if (args.size > 0)
	{
		// read file
		val lines = scala.io.Source.fromFile(args(0)).getLines()

		// converter function "64 70" => [64, 70]
		val fold = (l: List[Array[Int]], s: String) => s.split(" ").map(_.toInt) +: l

		// array will be in reverse order
		val arrays = lines.foldLeft(List[Array[Int]]())(fold)

		val start = System.currentTimeMillis()

		val result = arrays.drop(1).foldLeft(arrays(0))
		{
			(prevArr: Array[Int], arr: Array[Int]) => {
				for ( i <- 0 until arr.size )
				{
					arr(i) += scala.math.max(prevArr(i), prevArr(i+1))
				}
				arr
			}
		}

		println(result(0))
		println("Time spent: " + (System.currentTimeMillis() - start))
	}
	else
	{
		println("no filename");
	}
}