package gallia
package heads.reducing

import aptus.{Anything_, Seq_}

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
    def nums[T: WTT](optional: Boolean): Cls =
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

    def ints(optional: Boolean)(values: Values): Obj =
        counts(optional)(values)
      .merge(
        nums(optional)(ReducingTypeUtils._flattenedInts(values)))

    // ---------------------------------------------------------------------------
    def doubles(optional: Boolean)(values: Values): Obj =
        counts(optional)(values)
      .merge(
        nums(optional)(ReducingTypeUtils._flattenedDoubles(values)))

    // ---------------------------------------------------------------------------
    def strings(optional: Boolean)(values: Values): Obj =
          counts(optional)(values)
        .merge(
          obj(_values -> __strings(ReducingTypeUtils._strings(values))))

      // ---------------------------------------------------------------------------
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
          _.add(_present, values.flatten.size) }

    // ---------------------------------------------------------------------------
    // TODO: t210118084355 - _skewness, kurtosis (opt?) + mode, IQR + more percentiles + MAD/mean absolute deviation, trimmed
    private def nums[A: Numeric](optional: Boolean)(nums: List[A]): Obj =
      obj(
        _mean   -> nums.mean,
        _stdev  -> nums.stdev,

        _min    -> nums.min,
        _max    -> nums.max,

        _median -> nums.median)

  }
}

// ===========================================================================

