package gallia
package heads
package pivoting

import heads.TSL.Squash
import actions.ActionsOthers  
import selection.typed.TsBoilerplate.Squash

// ===========================================================================
private[pivoting] case class PivotingData[O1: WTT, D: WTT](
    target   : Squash.TSelector[O1], // see 200924162200

    aggOpt   : Option[Seq[O1] => D] = None,

    rows     : Renz = PivotingData.InitValue,
    column   : Key  = PivotingData.InitValue,
    newKeys  : Keyz = PivotingData.InitValue) {

  def pivone(input: HeadZ): HeadU = // TODO: t210303111953 - use different structure now
    input
      .ensureUniquenessBy(column) // 240124153043 - ensure uniquess first
      .zu { ActionsOthers.Pivone(
        newKeys,
        column,
        valueKey = target.pipe(Squash.resolve(_)).tq) }

  // ===========================================================================
  def pivot[O1: WTT, D: WTT](input: HeadZ): HeadZ =
    input // TODO: use cascade group by rather once done (see t210124100722)
      .groupBy(rows).as( _tmp1)
      .transform(_.entities(_tmp1)).using {
        _ .groupBy(column).as(_tmp2) // 200930125015 - this flattens, so must set defaults ahead of time if needed

          .pipe { headZ =>            
            aggOpt match {
              case None      => headZ.unnestOOO(_tmp2)
              case Some(agg) =>
                headZ
                  .transform(_.entities(_tmp2)).using {
                  _.squash(target) // compatible because of 200924162200
                    .using(agg) } } }

          .pivot(_tmp2).column(column).asNewKeys(newKeys.values) }
      .unnestAllFrom(_tmp1)
}      

// ---------------------------------------------------------------------------
object PivotingData { private val InitValue = null /* for now... */ }

// ===========================================================================
