package gallia.data.multiple

import aptus.Anything_
import gallia.heads.merging.MergingData._
import gallia.obj

// ===========================================================================
trait ObjsMerging { self: Objs =>

  // note, if reuse cogroup then must reinsert join key in its original place (TODO: worth bothering?)
  def join(joinType: JoinType, joinKeys: JoinKey)(that: Objs): Objs =
     (  ObjsMerging.pairs2(this.values, joinKeys.left),
        ObjsMerging.pairs1(that.values, joinKeys.right) )
      .thn { case (left, right) => left.join(joinType, gallia.data.single.ObjUtils.combine _)(right) }
      .map(_.get /* guaranteed by 201126124701 */)
      .thn(Objs.build)

  // ---------------------------------------------------------------------------
  def coGroup(joinType: JoinType, joinKeys: JoinKey, as: AsKeys)(that: Objs): Objs =
     (ObjsMerging.pairs1(this.values, joinKeys.left),
      ObjsMerging.pairs1(that.values, joinKeys.right) )
        .thn { case (left, right) => left.coGroup(joinType)(right) }
        .map { case (joinValueOpt: Option[AnyValue], (left: Iterable[Option[Obj]], right: Iterable[Option[Obj]])) =>
          obj(
              joinKeys.key -> joinValueOpt,

              // FIXME: t201205164730: may care about knowing there was a matching key, even if no other data...
              as.left  -> left .flatten,
              as.right -> right.flatten) }
        .thn(Objs.build)
}

// ===========================================================================
object ObjsMerging {
  private def pairs1(values: Streamer[Obj], joinKey: Key): Streamer[(Option[AnyValue], Option[Obj])] = values.map { o => o.opt(joinKey) -> o.removeOpt(joinKey) }
  private def pairs2(values: Streamer[Obj], joinKey: Key): Streamer[(Option[AnyValue], Option[Obj])] = values.map { o => o.opt(joinKey) -> Some(o) }
}

// ===========================================================================
