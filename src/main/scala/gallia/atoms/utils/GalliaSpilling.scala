package gallia
package atoms
package utils

import aptus.Line
import domain.GroupingPair._
import meta.PNF
import data.multiple.streamer._
import spilling._

// ===========================================================================
object GalliaSpilling {
  // see https://github.com/galliaproject/gallia-core#poor-mans-scaling-spilling
  private type OObj = Option[Obj]
  private type OVle = Option[Vle]

  // ===========================================================================
  def spillingSort(pnfs: Seq[PNF], sortFields: GnuSortFields)(c: Cls)(input: Streamer[Obj]): Streamer[Obj] =
    new SpillingSortSerDes(pnfs, c)
      .apply(input) {
        GnuSortByFirstFieldsHack.apply(Hacks.ExecutionContext, debug = "220705162301")(sortFields) }

  // ---------------------------------------------------------------------------
  def spillingSortAll(c: Cls)(descending: Boolean)(input: Streamer[Obj]): Streamer[Obj] =
    new SpillingSortAllSerDes(c)
      .apply(input) {
        GnuSortByAllHack.apply(Hacks.ExecutionContext, debug = "220705162302")(descending) }

  // ---------------------------------------------------------------------------
  def spillingSortUnsafe(c: Cls)(descending: Boolean)(input: Streamer[Obj]): Streamer[Obj] = ??? // TODO

  // ===========================================================================
  def spillingSortDistinct(c: Cls)(input: Streamer[Obj]): Streamer[Obj] =
    new SpillingSortAllSerDes(c) // can reuse this serdes
      .apply(input) {
        GnuSortUniqHack.apply(Hacks.ExecutionContext, debug = "220705162303") }

  // ===========================================================================
  def spillingGroupBy1N(pair: GroupingPairN1)(input: Streamer[(OObj, OVle)]): Streamer[(OObj, List[OVle])] =
    new SpillingGroupBy1NSerDes(pair)
      .apply(input) {
        GnuSortByFirstFieldsHack.default(Hacks.ExecutionContext, debug = "220705162304") }
      .groupByPreSortedKey

  // ---------------------------------------------------------------------------
  def spillingGroupBy1N(pair: GroupingPair1N)(input: Streamer[(OVle, OObj)]): Streamer[(OVle, List[OObj])] =
    new SpillingGroupByN1SerDes(pair)
      .apply(input) {
        GnuSortByFirstFieldsHack.default(Hacks.ExecutionContext, debug = "220705162305") }
      .groupByPreSortedKey

  // ---------------------------------------------------------------------------
  def spillingGroupByNN(pair: GroupingPairNN)(input: Streamer[(OObj, OObj)]): Streamer[(OObj, List[OObj])] =
    new SpillingGroupByNNSerDes(pair)
      .apply(input) {
        GnuSortByFirstFieldsHack.default(Hacks.ExecutionContext, debug = "220705162306") }
      .groupByPreSortedKey

  // ===========================================================================
  def spillingJoin(left: Streamer[Line], right: Streamer[Line]): aptus.CloseabledIterator[Line] =
    GnuJoinByFirstFieldHack.apply(Hacks.ExecutionContext)(
      left .closeabledIterator,
      right.closeabledIterator)

}

// ===========================================================================