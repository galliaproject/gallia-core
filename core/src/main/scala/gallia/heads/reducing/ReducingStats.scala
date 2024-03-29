package gallia
package heads
package reducing

import aptus.Anything_

import meta._

// ===========================================================================
private[reducing] object ReducingStats { // 210118083814
  private val _present  = Symbol("_present")
  private val _distinct = Symbol("_distinct")
  private val _value    = Symbol("_value")

  // ---------------------------------------------------------------------------
  //TODO:
  // - t201207170908 - case classes mapping to/from

  // ===========================================================================
  object Meta { // TODO: Cls.FullDescriptiveStats

    private def counts(optional: Boolean): Cls =
      cls(
          _count_all.int,
          _distinct .int)
        .pipeIf(optional) {
          _.add(_present  .int) }

    // ---------------------------------------------------------------------------
    def nums[T : WTT : Numeric](optional: Boolean): Cls =
      counts(optional)
          .merge(
        cls(
          _mean      .double,
          _stdev     .double,

          _min       .typed[T],
          _max       .typed[T],

          _median    .double))

    // ---------------------------------------------------------------------------
    // t201209145048 - string stats: allow only doing top/bottom n
    def strings(optional: Boolean): Cls =
      counts(optional)
          .merge(
        if (optional) cls(_values.cls(_value.string_, _count_all.int))
        else          cls(_values.cls(_value.string , _count_all.int)) )

  }

  // ===========================================================================
  object Data {

    def ints   (optional: Boolean)(values: Values): Obj = counts(optional)(values).merge(nums(optional)(ReducingTypeUtils._flattenedInts   (values)))
    def doubles(optional: Boolean)(values: Values): Obj = counts(optional)(values).merge(nums(optional)(ReducingTypeUtils._flattenedDoubles(values)))
    
    def bytes  (optional: Boolean)(values: Values): Obj = counts(optional)(values).merge(nums(optional)(ReducingTypeUtils._flattenedBytes  (values)))
    def shorts (optional: Boolean)(values: Values): Obj = counts(optional)(values).merge(nums(optional)(ReducingTypeUtils._flattenedShorts (values)))
    def longs  (optional: Boolean)(values: Values): Obj = counts(optional)(values).merge(nums(optional)(ReducingTypeUtils._flattenedLongs  (values)))
    def floats (optional: Boolean)(values: Values): Obj = counts(optional)(values).merge(nums(optional)(ReducingTypeUtils._flattenedFloats (values)))
        
    def bigInts(optional: Boolean)(values: Values): Obj = counts(optional)(values).merge(nums(optional)(ReducingTypeUtils._flattenedBigInts(values)))
    def bigDecs(optional: Boolean)(values: Values): Obj = counts(optional)(values).merge(nums(optional)(ReducingTypeUtils._flattenedBigDecs(values)))

    // ---------------------------------------------------------------------------
    def strings(optional: Boolean)(values: Values): Obj = counts(optional)(values).merge(obj(_values -> __strings(ReducingTypeUtils._strings(values))))

    // ===========================================================================
    private def __strings(values: List[Option[String]]): Seq[Obj] =
        values
          .groupBy(identity)
          .mapValues(_.size)
          .toSeq
          .sortBy { case (valueOpt, count) => (-count ,valueOpt) }
          .map { case (valueOpt, count) =>
            valueOpt match {
              case None        => obj(                 _count_all -> count)
              case Some(value) => obj(_value -> value, _count_all -> count) } }

    // ===========================================================================
    private def counts(optional: Boolean)(values: Values): Obj =
      obj(
          _count_all -> values         .size,
          _distinct  -> values.distinct.size)
        .pipeIf(optional) {
          _.addEntry(_present, values.flatten.size) }

    // ---------------------------------------------------------------------------
    // TODO: t210118084355 - _skewness, kurtosis (opt?) + mode, IQR + more percentiles + MAD/mean absolute deviation, trimmed
    private def nums[A: Numeric](optional: Boolean)(nums: List[A]): Obj = {
      if (nums.size == 1) { // not handled well by DescriptiveStatistics
        val sole = nums.head
    
        obj(
          _mean   -> sole,
          _stdev  -> 0.0,
    
          _min    -> sole,
          _max    -> sole,
    
          _median -> sole)    
      } else {
        val num = implicitly[Numeric[A]]                      
        val ds =
          new org.apache.commons.math3.stat.descriptive.DescriptiveStatistics(
            nums.map(num.toDouble).toArray)

        obj(
          _mean   -> ds.getMean,
          _stdev  -> ds.getStandardDeviation, // note: returns sample's, ie bias-corrected, TODO: t220330150326- consider squaring(getPopulationVariance) instead? 
    
          _min    -> ds.getMin,
          _max    -> ds.getMax,
    
          _median -> ds.getPercentile(50))
      }
    }
  }
}

// ===========================================================================

