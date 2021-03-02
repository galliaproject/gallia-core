package gallia.data.multiple.streamer

import scala.util.chaining.scalaUtilChainingOps // trying it out
import scala.reflect.{ClassTag => CT}

import aptus.Seq_
import aptus.utils.MapUtils

import gallia.heads.merging.MergingData._

// ===========================================================================
object ViewStreamerUtils {
  type DataRepr[T] = cross.SeqView[T] // TODO: t210115103554 - confirm reads entire Seq once first (so can redo as needed)?

  // ===========================================================================
  private[streamer] def groupByKey[K, V](list: DataRepr[(K, V)]): DataRepr[(K, List[V])] =
    MapUtils
      .groupByKeyWithListMap(list)
      .toList.view
      .map { case (key, values) =>
        key -> values.toList }

  // ===========================================================================
  private[streamer] def union[B: CT](dis: ViewStreamer[B], that: Streamer[B]): Streamer[B] =
    that.tipe match {
      case StreamerType.ViewBased =>
        Streamer.fromList(dis.toList ++ that.toList)

      // ---------------------------------------------------------------------------
      case StreamerType.IteratorBased | StreamerType.RDDBased => // delegate
        val left : Streamer[B] = this.asInstanceOf[Streamer[B]]
        val right: Streamer[B] = that.asInstanceOf[Streamer[B]]

        left
          .pipe(right.asMeBased)
          .union(right)
    }

  // ===========================================================================
  private[streamer] def coGroup[K: CT, V: CT]
          (joinType: JoinType)
          (left: Streamer[(K, V)], right: Streamer[(K, V)])
        : Streamer[(K, (Iterable[V], Iterable[V]))] =
      right.tipe match {
        case StreamerType.ViewBased =>
            _coGroup(joinType)(
                // note: n210302094313 - the ListMap aspect comes with a 50% time increase...
                left .toList.groupByKeyWithListMap,
                right.toList.groupByKeyWithListMap)
              .pipe(new ViewStreamer(_))

        // ---------------------------------------------------------------------------
        case StreamerType.IteratorBased | StreamerType.RDDBased => // delegate
          left
            .pipe(right.asMeBased)
            .coGroup(joinType)(right)
      }

    // ===========================================================================
    private[streamer] def join[K: CT, V: CT]
          (joinType: JoinType, combine: (V, V) => V)
          (left: Streamer[(K, V)], right: Streamer[(K, V)])
        : Streamer[V] =
      right.tipe match {

        case StreamerType.ViewBased =>
          _join(joinType, combine)(
                // note: n210302094313 - the ListMap aspect comes with a 50% time increase...
                left .toList.groupByKeyWithListMap,
                right.toList.groupByKeyWithListMap)
            .pipe(new ViewStreamer(_))

        // ---------------------------------------------------------------------------
        case StreamerType.IteratorBased | StreamerType.RDDBased => ??? // delegate
      }

  // ===========================================================================
  private def _coGroup[K, V]
          (joinType: JoinType)
          ( leftLookup: Map[K, Iterable[V]],
           rightLookup: Map[K, Iterable[V]])
        : DataRepr[(K, (Iterable[V], Iterable[V]))] =
      _joinValues(joinType)(
           leftLookup.keys.toSeq.distinct,
          rightLookup.keys.toSeq.distinct)
        .toList.view
        .map { joinValueOpt =>
          ( joinValueOpt,
             leftLookup.get(joinValueOpt).getOrElse(Nil) ->
            rightLookup.get(joinValueOpt).getOrElse(Nil)) }

  // ===========================================================================
  private def _join[K, V]
          (joinType: JoinType, combiner: (V, V) => V)
          ( leftLookup: Map[K, Iterable[V]],
           rightLookup: Map[K, Iterable[V]])
        : DataRepr[V] =
      _joinValues(joinType)(
           leftLookup.keys.toSeq.distinct,
          rightLookup.keys.toSeq.distinct)
        .toSeq.view
        .flatMap { joinValueOpt =>
          val  left: Seq[V] =  leftLookup.get(joinValueOpt).toList.flatten
          val right: Seq[V] = rightLookup.get(joinValueOpt).toList.flatten

          // ---------------------------------------------------------------------------
               if (left .isEmpty) right
          else if (right.isEmpty) left
          else for { l <- left; r <- right }
            yield { combiner(l, r) } } // 201126124701 - can't both be empty (by design)
        .toSeq.view

  // ===========================================================================
  private def _joinValues[K](joinType: JoinType)(left: Seq[K], right: Seq[K]): Seq[K] = // TODO: or as set (but loses order)
    (joinType match {
          case JoinType.full  => left.union    (right)
          case JoinType.left  => left
          case JoinType.right =>                right
          case JoinType.inner => left.intersect(right) })
       .distinct

}

// ===========================================================================
