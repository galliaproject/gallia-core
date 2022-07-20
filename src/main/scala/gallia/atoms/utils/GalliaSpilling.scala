package gallia
package atoms
package utils

import aptus._
import domain.GroupingPair._
import meta.PNF
import streamer.{IteratorStreamer, Streamer}
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
  def spillingJoin
        (leftPair: GroupingPair1N, rightPair: GroupingPair1N)
        (left: Streamer[(OVle, OObj)], right: Streamer[(OVle, OObj)])
      : Streamer[Obj] = {

    val     leftSerializer = new SpillingGroupByN1SerDes(leftPair)
    val    rightSerializer = new SpillingGroupByN1SerDes(rightPair)
    val outputDeserializer = new SpillingJoinDeserializer(leftPair.groupees, rightPair.groupees, rightPair.grouper.key)

    // ---------------------------------------------------------------------------
    new data.DataRegenerationClosure[Obj] {
        def regenerate = { () =>

          // ---------------------------------------------------------------------------
          GnuJoinByFirstFieldHack
            .apply(Hacks.ExecutionContext)( // will schedule to close inputs accordingly
              sideInput( leftSerializer)(left) .closeabledIterator,
              sideInput(rightSerializer)(right).closeabledIterator)
            .map(outputDeserializer._deserialize)
            .flatMap { case (ls, rs) =>
              for (l <- ls; r <- rs) yield
                l merge r } }

        // ---------------------------------------------------------------------------
        def sideInput(serializer: SpillingGroupByN1SerDes)(input : Streamer[(OVle, OObj)]): IteratorStreamer[String] =
          input
              .asInstanceOfIteratorStreamer
              ._map  (serializer._serialize)
              ._alter(GnuSortByFirstFieldsHack.default(Hacks.ExecutionContext, debug = "220720113328"))
              ._map  (_.splitBy(serializer.pairSeparator).force.tuple2)
              ._alter(_.groupByPreSortedKey)
              ._map  ((SpillingJoinDeserializer.postGroupingSerialization _).tupled) }
      .pipe(IteratorStreamer.from)
  }
}

// ===========================================================================