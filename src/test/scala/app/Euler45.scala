package app

/**
 * User: Eugene Dubrovka
 * Date: 11/2/11
 * Time: 1:25 PM
 */

object Euler45 extends App
{
	type ValueType = BigInt
	type ValueStream = Stream[ValueType]

	val triangle = (n: ValueType) => n * (n + 1) / 2
	val pentagon = (n: ValueType) => n * (3*n - 1) / 2
	val hexagon = (n: ValueType) => n * (2*n - 1)

	def ngonstream(fNext: (ValueType) => ValueType): ValueStream =
	{
		def next(idx: ValueType): ValueStream =
		{
			Stream.cons(fNext(idx), next(idx + 1))
		}
		next(1)
	}

	def triangleNumbersStream: ValueStream =
	{
		def next(ts: ValueStream, ps: ValueStream, hs: ValueStream): ValueStream =
		{
			var found = false

			var vts = ts
			var vps = ps
			var vhs = hs

			var ctv = vts.take(1)(0)
			var cpv = vps.take(1)(0)
			var chv = vhs.take(1)(0)

			while (!found)
			{
				vts = vts dropWhile { x => x < cpv || x < chv }
				vps = vps dropWhile { x => x < ctv || x < chv }
				vhs = vhs dropWhile { x => x < ctv || x < cpv }

				ctv = vts.take(1)(0)
				cpv = vps.take(1)(0)
				chv = vhs.take(1)(0)

				found = ctv == cpv && cpv == chv
			}

 	        Stream.cons(ctv, next(vts.drop(1), vps.drop(1), vhs.drop(1)))
		}

		next(ngonstream(triangle), ngonstream(pentagon), ngonstream(hexagon))
	}

	triangleNumbersStream.dropWhile(_ <= 40755) take(1) foreach print
}