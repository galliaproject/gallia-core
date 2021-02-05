package gallia.heads.pivoting

import aptus.Anything_
import gallia._

// ===========================================================================
private[pivoting] object PivotingCombos {
  val DefaultKeySeparator = "_" //TODO: t210117192221 - configurable key separator

  // ---------------------------------------------------------------------------
  def apply1[O1: WTT, D: WTT](self: HeadZ, data: Data1[O1, D]): HeadZ =
    _pivoting(self)(data.rows, data.columns, data.newKeys) { key =>
      headZ =>
        data.aggOpt match {
          case None      => headZ.unnestOOO(key)
          case Some(agg) =>
            headZ
              .transform(_.objz(key)).using {
              _.squash(data.target1) // compatible because of 200924162200
                .using(agg) } } }

  // ---------------------------------------------------------------------------
  def apply2[O1: WTT, O2: WTT, D: WTT](self: HeadZ, data: Data2[O1, O2, D]): HeadZ =
    _pivoting(self)(data.rows, data.columns, data.newKeys) { key =>
      _.transform(_.objz(key)).using {
        _.squash(data.target1, data.target2) // compatible because of 200924162200
          .using(data.agg) } }

  // ---------------------------------------------------------------------------
  def apply3[O1: WTT, O2: WTT, O3: WTT, D: WTT](self: HeadZ, data: Data3[O1, O2, O3, D]): HeadZ =
    _pivoting(self)(data.rows, data.columns, data.newKeys) { key =>
      _.transform(_.objz(key)).using {
        _.squash(data.target1, data.target2, data.target3) // compatible because of 200924162200
          .using(data.agg) } }

  // ---------------------------------------------------------------------------
  def apply4[O1: WTT, O2: WTT, O3: WTT, O4: WTT, D: WTT](self: HeadZ, data: Data4[O1, O2, O3, O4, D]): HeadZ =
    _pivoting(self)(data.rows, data.columns, data.newKeys) { key =>
      _.transform(_.objz(key)).using {
        _.squash(data.target1, data.target2, data.target3, data.target4) // compatible because of 200924162200
          .using(data.agg) } }

  // ---------------------------------------------------------------------------
  def apply5[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT, D: WTT](self: HeadZ, data: Data5[O1, O2, O3, O4, O5, D]): HeadZ =
    _pivoting(self)(data.rows, data.columns, data.newKeys) { key =>
      _.transform(_.objz(key)).using {
        _.squash(data.target1, data.target2, data.target3, data.target4, data.target5) // compatible because of 200924162200
          .using(data.agg) } }

  // ===========================================================================
  private[heads] def _pivoting(self: HeadZ)
        (rows    : Renz,
         columns : Keyz,
         newKeys : Keyz)
        (agg: Key => HeadZ => HeadZ)
      : HeadZ = {
    val keySeparator = DefaultKeySeparator

    self // TODO: use cascade group by rather (once done)
      .groupBy(rows).as( _tmp1)
      .transform(_.objz(_tmp1)).using {
        _ .groupBy(columns).as(_tmp2) // 200930125015 - this flattens, so must set defaults ahead of time if needed
          .thn(agg(            _tmp2))
          .unarrayEntries(columns)
            .asNewKeys(newKeys.values)
                .withKeySeparator(keySeparator)
              .valueKey(_tmp2) }
      .unnestAllFrom(_tmp1)
  }

}

// ===========================================================================
