package gallia
package data
package multiple

import aptus.Anything_
import aptus.Line

import domain.GroupingPair.GroupingPair1N
import heads.merging.MergingData._
import atoms.utils.{GalliaSpilling, SpillingJoinSerializer, SpillingJoinDeserializer}

// ===========================================================================
trait ObjsMerging { self: Objs =>

  // ---------------------------------------------------------------------------
  // note, if reuse cogroup then must reinsert join key in its original place (TODO: worth bothering?)
  def join
        (leftCls: Cls, rightCls: Cls)
        (joinType: JoinType, joinKeys: JoinKey)
        (that: Objs)
        : Objs =
      if (this.isIteratorBased && that.isIteratorBased) spillingJoin(leftCls, rightCls)(joinType, joinKeys)(that)
      else                                              inMemoryjoin                   (joinType, joinKeys)(that)

    // ---------------------------------------------------------------------------
    private def inMemoryjoin(joinType: JoinType, joinKeys: JoinKey)(that: Objs): Objs =
       (  ObjsMerging.pairs2(this.values, joinKeys.left),
          ObjsMerging.pairs2(that.values, joinKeys.right) )
        .pipe { case (left, right) => left.join(joinType, single.ObjUtils.combine(joinKeys.right) _)(right) }
        .map(_.get /* guaranteed by 201126124701 */)
        .pipe(Objs.build)

    // ---------------------------------------------------------------------------
    private def spillingJoin(leftCls: Cls, rightCls: Cls)(joinType: JoinType, joinKeys: JoinKey)(that: Objs): Objs = {

      val  leftPair = GroupingPair1N( leftCls.field(joinKeys.left) , groupees =  leftCls)
      val rightPair = GroupingPair1N(rightCls.field(joinKeys.right), groupees = rightCls)

      // ---------------------------------------------------------------------------
      val  leftSerializer = new SpillingJoinSerializer(joinKeys.left .pipe( leftCls.field), leftCls)
      val rightSerializer = new SpillingJoinSerializer(joinKeys.right.pipe(rightCls.field), rightCls)

      val deserializer    = new SpillingJoinDeserializer(leftPair.groupees, rightPair.groupees, rightPair.grouper.key)

      // ===========================================================================
      new DataRegenerationClosure[Obj] {

        // ---------------------------------------------------------------------------
        def inputLines(pair: GroupingPair1N, serializer: SpillingJoinSerializer, rhs: Boolean)(input: Streamer[Obj]): Streamer[Line] =
          input
            .map { o =>
              o.attemptKey(pair.grouper.key) ->
              o.in.some }
            .pipe(GalliaSpilling.spillingGroupBy1N(pair))
            .map(serializer._serialize)

        // ---------------------------------------------------------------------------
        def regenerate: () => aptus.CloseabledIterator[Obj] = () =>
          (   inputLines(leftPair,  leftSerializer , rhs = false)(self.values),
              inputLines(rightPair, rightSerializer, rhs = true )(that.values))
            .pipe((GalliaSpilling.spillingJoin _).tupled)
            .map(deserializer._deserialize)
            .flatMap { case (ls, rs) =>
              for (l <- ls; r <- rs) yield (l.merge(r)) } }
          .pipe(Objs.from4)
    }

  // ===========================================================================
  def coGroup(joinType: JoinType, joinKeys: JoinKey, as: AsKeys)(that: Objs): Objs =
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
}

// ===========================================================================
object ObjsMerging {
  private def pairs1(values: Streamer[Obj], joinKey: Key): Streamer[(Option[AnyValue], Option[Obj])] = values.map { o => o.attemptKey(joinKey) -> o.removeOpt(joinKey) }
  private def pairs2(values: Streamer[Obj], joinKey: Key): Streamer[(Option[AnyValue], Option[Obj])] = values.map { o => o.attemptKey(joinKey) -> Some(o) }
}

// ===========================================================================
