package gallia
package atoms
package utils

import aptus._
import domain.GroupingPair._
import heads.merging.MergingData.{AsKeys, JoinKey}
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
        GnuSortByFirstFieldsHack.apply(Hacks.galliaExecutionContext.forceValue(), debug = "220705162301")(sortFields) }

  // ---------------------------------------------------------------------------
  def spillingSortAll(c: Cls)(descending: Boolean)(input: Streamer[Obj]): Streamer[Obj] =
    new SpillingSortAllSerDes(c)
      .apply(input) {
        GnuSortByAllHack.apply(Hacks.galliaExecutionContext.forceValue(), debug = "220705162302")(descending) }

  // ---------------------------------------------------------------------------
  def spillingSortUnsafe(c: Cls)(descending: Boolean)(input: Streamer[Obj]): Streamer[Obj] = ??? // TODO

  // ===========================================================================
  def spillingSortDistinct(c: Cls)(input: Streamer[Obj]): Streamer[Obj] =
    new SpillingSortAllSerDes(c) // can reuse this serdes
      .apply(input) {
        GnuSortUniqHack.apply(Hacks.galliaExecutionContext.forceValue(), debug = "220705162303") }

  // ===========================================================================
  def spillingGroupBy1N(pair: GroupingPairN1)(input: Streamer[(OObj, OVle)]): Streamer[(OObj, List[OVle])] =
    new SpillingGroupBy1NSerDes(pair)
      .apply(input) {
        GnuSortByFirstFieldsHack.default(Hacks.galliaExecutionContext.forceValue(), debug = "220705162304") }
      .groupByPreSortedKey

  // ---------------------------------------------------------------------------
  def spillingGroupBy1N(pair: GroupingPair1N)(input: Streamer[(OVle, OObj)]): Streamer[(OVle, List[OObj])] =
    new SpillingGroupByN1SerDes(pair)
      .apply(input) {
        GnuSortByFirstFieldsHack.default(Hacks.galliaExecutionContext.forceValue(), debug = "220705162305") }
      .groupByPreSortedKey

  // ---------------------------------------------------------------------------
  def spillingGroupByNN(pair: GroupingPairNN)(input: Streamer[(OObj, OObj)]): Streamer[(OObj, List[OObj])] =
    new SpillingGroupByNNSerDes(pair)
      .apply(input) {
        GnuSortByFirstFieldsHack.default(Hacks.galliaExecutionContext.forceValue(), debug = "220705162306") }
      .groupByPreSortedKey

  // ===========================================================================
  def spillingJoin
          (leftPair: GroupingPair1N, rightPair: GroupingPair1N)
          (left: Streamer[(OVle, OObj)], right: Streamer[(OVle, OObj)])
        : Streamer[Obj] = {
      val rightGrouper = rightPair.grouper.key

      // ---------------------------------------------------------------------------
      _spillingJoinOrCoGroup(leftPair, rightPair)(left, right) {
        _.flatMap { case (ls, rs) =>
          for (l <- ls; r <- rs.map(_.remove(rightGrouper))) yield
            l merge r } } }

    // ---------------------------------------------------------------------------
    def spillingCoGroup
          (leftPair: GroupingPair1N, rightPair: GroupingPair1N, joinKeys: JoinKey, as: AsKeys)
          (left: Streamer[(OVle, OObj)], right: Streamer[(OVle, OObj)])
        : Streamer[Obj] = {
      val  leftGrouper =  leftPair.grouper.key
      val rightGrouper = rightPair.grouper.key

      // ---------------------------------------------------------------------------
      _spillingJoinOrCoGroup(leftPair, rightPair)(left, right) {
        _.map { case (ls, rs) =>
          obj(joinKeys.key -> ls.head.attemptKey(leftGrouper),
            as.left  -> ls.map(_.remove( leftGrouper)),
            as.right -> rs.map(_.remove(rightGrouper))) } } }

    // ---------------------------------------------------------------------------
    private def _spillingJoinOrCoGroup
          (leftPair: GroupingPair1N, rightPair: GroupingPair1N)
          (left: Streamer[(OVle, OObj)], right: Streamer[(OVle, OObj)])
          (f: CloseabledIterator[(List[Obj], List[Obj])] => CloseabledIterator[Obj])
        : Streamer[Obj] = {
      val     leftSerializer = new SpillingGroupByN1SerDes(leftPair)
      val    rightSerializer = new SpillingGroupByN1SerDes(rightPair)
      val outputDeserializer = new SpillingJoinDeserializer(leftPair.groupees, rightPair.groupees)

      // ---------------------------------------------------------------------------
      new data.DataRegenerationClosure[Obj] {

          def regenerate = { () =>
            GnuJoinByFirstFieldHack
              .apply(Hacks.galliaExecutionContext.forceValue())( // will schedule to close inputs accordingly
                sideInput( leftSerializer)(left) .closeabledIterator,
                sideInput(rightSerializer)(right).closeabledIterator)
              .map(outputDeserializer._deserialize)
              .pipe(f) }

          // ---------------------------------------------------------------------------
          def sideInput(serializer: SpillingGroupByN1SerDes)(input : Streamer[(OVle, OObj)]): IteratorStreamer[String] =
            input
                .asInstanceOfIteratorStreamer
                ._map  (serializer._serialize)
                ._alter(GnuSortByFirstFieldsHack.default(Hacks.galliaExecutionContext.forceValue(), debug = "220720113328"))
                ._map  (_.splitBy(serializer.pairSeparator).force.tuple2)
                ._alter(_.groupByPreSortedKey)
                ._map  ((SpillingJoinDeserializer.postGroupingSerialization _).tupled) }
        .pipe(IteratorStreamer.from) }

}

// ===========================================================================
