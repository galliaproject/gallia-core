package gallia
package data
package multiple

import aptus.Anything_
import domain.GroupingPair.GroupingPair1N
import heads.merging.MergingData._
import atoms.utils.GalliaSpilling
import gallia.streamer.StreamerType

// ===========================================================================
trait ObjsMerging { self: Objs =>

  // 220721102702 - note, if reuse cogroup then must reinsert join key in its original place (TODO: worth bothering?)
  def join
          (leftCls: Cls, rightCls: Cls)
          (joinType: JoinType, joinKeys: JoinKey)
          (that: Objs)
        : Objs = {
      val (spilling, (leftStreamer, rightStreamer)) = StreamerType.spillingTriplet(left = self, right = that)

      if (spilling) leftStreamer.spillingJoin(leftCls, rightCls)(joinType, joinKeys)(rightStreamer)
      else          leftStreamer.streamerJoin                   (joinType, joinKeys)(rightStreamer)
    }

    // ---------------------------------------------------------------------------
    private[ObjsMerging] def streamerJoin(joinType: JoinType, joinKeys: JoinKey)(that: Objs): Objs =
       (  ObjsMerging.pairs2(this.values, joinKeys.left),
          ObjsMerging.pairs2(that.values, joinKeys.right) )
        .pipe { case (left, right) => left.join(joinType, single.ObjUtils.combine(joinKeys.right) _)(right) }
        .map(_.get /* guaranteed by 201126124701 */)
        .pipe(Objs.build)

    // ---------------------------------------------------------------------------
    private[ObjsMerging] def spillingJoin(leftCls: Cls, rightCls: Cls)(joinType: JoinType, joinKeys: JoinKey)(that: Objs): Objs = {
      val  leftPair = GroupingPair1N( leftCls.field(joinKeys.left) , groupees =  leftCls)
      val rightPair = GroupingPair1N(rightCls.field(joinKeys.right), groupees = rightCls)

      // ---------------------------------------------------------------------------
      GalliaSpilling.spillingJoin(leftPair, rightPair)(
        this.values.map { o => o.attemptKey( leftPair.grouper.key) -> o.in.some },
        that.values.map { o => o.attemptKey(rightPair.grouper.key) -> o.in.some } )
      .pipe(Objs.build)
    }

  // ===========================================================================
  def coGroup
          (leftCls: Cls, rightCls: Cls)
          (joinType: JoinType, joinKeys: JoinKey, as: AsKeys)
          (that: Objs)
        : Objs = {
      val (spilling, (leftStreamer, rightStreamer)) = StreamerType.spillingTriplet(left = self, right = that)

      if (spilling) leftStreamer.spillingCoGroup(leftCls, rightCls)(joinType, joinKeys, as)(rightStreamer)
      else          leftStreamer.streamerCoGroup                   (joinType, joinKeys, as)(rightStreamer)
    }

    // ---------------------------------------------------------------------------
    private[ObjsMerging] def streamerCoGroup(joinType: JoinType, joinKeys: JoinKey, as: AsKeys)(that: Objs): Objs =
     (ObjsMerging.pairs1(this.values, joinKeys.left),
      ObjsMerging.pairs1(that.values, joinKeys.right) )
        .pipe { case (left, right) => left.coGroup(joinType)(right) }
        .map { case (joinValueOpt: Option[AnyValue], (left: Iterable[Option[Obj]], right: Iterable[Option[Obj]])) =>
          obj(
              joinKeys.key -> joinValueOpt,

              // FIXME: t201205164730: may care about knowing there was a matching key, even if no other data...
              as.left  -> left .flatten,
              as.right -> right.flatten) }
        .pipe(Objs.build)

    // ---------------------------------------------------------------------------
    private[ObjsMerging] def spillingCoGroup(leftCls: Cls, rightCls: Cls)(joinType: JoinType, joinKeys: JoinKey, as: AsKeys)(that: Objs): Objs = {
      val  leftPair = GroupingPair1N( leftCls.field(joinKeys.left) , groupees =  leftCls)
      val rightPair = GroupingPair1N(rightCls.field(joinKeys.right), groupees = rightCls)

      // ---------------------------------------------------------------------------
      GalliaSpilling.spillingCoGroup(leftPair, rightPair, joinKeys, as)(
        this.values.map { o => o.attemptKey( leftPair.grouper.key) -> o.in.some },
        that.values.map { o => o.attemptKey(rightPair.grouper.key) -> o.in.some } )
      .pipe(Objs.build)
    }

}

// ===========================================================================
object ObjsMerging {

  private def pairs1(values: Streamer[Obj], joinKey: Key): Streamer[(Option[AnyValue], Option[Obj])] = values.map { o => o.attemptKey(joinKey) -> o.removeOpt(joinKey) }
  private def pairs2(values: Streamer[Obj], joinKey: Key): Streamer[(Option[AnyValue], Option[Obj])] = values.map { o => o.attemptKey(joinKey) -> Some(o) }

}

// ===========================================================================
