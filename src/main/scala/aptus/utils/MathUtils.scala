package aptus.utils

import aptus.Anything_

// ===========================================================================
object MathUtils {

  def stdev[A](coll: Seq[A], mean: Double)(implicit num: Numeric[A]): Double =
    ((coll
          .iterator
          .map(num.toDouble)
          .map(_ - mean)
          .map(math.pow(_, 2)) // .squared
          .sum) /
        coll.size)
      .thn(math.sqrt) // .squareRooted

  // ---------------------------------------------------------------------------
  def percentile[A](coll: Seq[A], n: Double)(implicit num: Numeric[A]): Double =
    coll
      .map(num.toDouble)
      .toArray
      .thn(new org.apache.commons.math3.stat.descriptive.DescriptiveStatistics(_))
      .getPercentile(n.require(_ >= 0).require(_ <= 100))

}

// ===========================================================================
