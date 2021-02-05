package gallia.data.multiple.streamer

import scala.reflect.{ClassTag => CT}

import aptus.{Anything_, Seq_}
import gallia.heads.merging.MergingData._

// ===========================================================================
object ViewStreamerUtils {
  type DataRepr[T] = scala.collection.SeqView[T, Seq[_]] // TODO: t210115103554 - confirm reads entire Seq once first (so can redo as needed)?

  // ===========================================================================
  private[streamer] def groupByKey[K, V](list: DataRepr[(K, V)]): DataRepr[(K, List[V])] =
    list
      .groupByKeyWithListMap
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
          .thn(right.asMeBased)
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
                left .toList.groupByKey,
                right.toList.groupByKey)
              .thn(new ViewStreamer(_))

        // ---------------------------------------------------------------------------
        case StreamerType.IteratorBased | StreamerType.RDDBased => // delegate
          left
            .thn(right.asMeBased)
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
                left.toList.groupByKey,
                right.toList.groupByKey)
            .thn(new ViewStreamer(_))

        // ---------------------------------------------------------------------------
        case StreamerType.IteratorBased | StreamerType.RDDBased => ??? // delegate
      }

  // ===========================================================================
  private def _coGroup[K, V]
          (joinType: JoinType)
          ( leftLookup: Map[K, Iterable[V]],
           rightLookup: Map[K, Iterable[V]])
        : DataRepr[(K, (Iterable[V], Iterable[V]))] =
      _joinValueSet(joinType)(
           leftLookup.keySet,
          rightLookup.keySet)
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
      _joinValueSet(joinType)(
           leftLookup.keySet,
          rightLookup.keySet)
        .toList.view
        .flatMap { joinValueOpt =>
          val  left: Seq[V] =  leftLookup.get(joinValueOpt).toList.flatten
          val right: Seq[V] = rightLookup.get(joinValueOpt).toList.flatten

          // ---------------------------------------------------------------------------
               if (left .isEmpty) right
          else if (right.isEmpty) left
          else for { l <- left; r <- right }
            yield { combiner(l, r) } } // 201126124701 - can't both be empty (by design)

  // ===========================================================================
  private def _joinValueSet[K](joinType: JoinType)(left: Set[K], right: Set[K]): Set[K] =
    joinType match {
        case JoinType.full  => left.union    (right)
        case JoinType.left  => left
        case JoinType.right =>                right
        case JoinType.inner => left.intersect(right) } // TODO: re-sort?

}

// ===========================================================================
