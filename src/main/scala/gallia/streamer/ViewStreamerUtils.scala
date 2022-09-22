package gallia
package streamer

import scala.reflect.{ClassTag => CT}

import aptus.Iterator_

import heads.merging.MergingData._

// ===========================================================================
object ViewStreamerUtils {

  // note: n210302094313 - the ListMap aspect comes with a 50% time increase...  
  @inline private def keyGrouping[K, V](itr: Iterator[(K, V)]): Map[K, Seq[V]] =
    if (gallia.Hacks.loseOrderOnGrouping.test()) itr.groupByKey
    else                                         itr.groupByKeyWithListMap

  // ===========================================================================
  private[streamer] def groupByKey[K, V](view: ViewRepr[(K, V)]): ViewRepr[(K, List[V])] =
    view.iterator
      .pipe(keyGrouping)
      .toSeq.view
      .map { case (key, values) =>
        key -> values.toList }

  // ===========================================================================
  private[streamer] def union[B: CT](dis: ViewStreamer[B], that: Streamer[B]): Streamer[B] =
    that.tipe match {
      case StreamerType.ViewBased =>
        //Streamer.fromView(dis.toView ++ that.toView) - FIXME: 2.13 issues
        ViewStreamer.from(dis.toList ++ that.toList)

      // ---------------------------------------------------------------------------
      case StreamerType.IteratorBased | StreamerType.RDDBased => // delegate
        val left : Streamer[B] = this.asInstanceOf[Streamer[B]]
        val right: Streamer[B] = that.asInstanceOf[Streamer[B]]

        left
          .pipe(right.toMeBased)
          .union(right)
    }

  // ===========================================================================
  private[streamer] def zip[B: CT](dis: ViewStreamer[B], that: Streamer[B], combiner: (B, B) => B): Streamer[B] =
    that.tipe match {
      case StreamerType.ViewBased =>
        //Streamer.fromView(dis.toView ++ that.toView) - FIXME: 2.13 issues
        ViewStreamer
          .from(dis.toList zip that.toList)
          .map(combiner.tupled)

      // ---------------------------------------------------------------------------
      case StreamerType.IteratorBased | StreamerType.RDDBased => // delegate
        val left : Streamer[B] = this.asInstanceOf[Streamer[B]]
        val right: Streamer[B] = that.asInstanceOf[Streamer[B]]

        left
          .pipe(right.toMeBased)
          .zip (right, combiner)
    }

  // ===========================================================================
  private[streamer] def coGroup[K: CT, V: CT]
          (joinType: JoinType)
          (left: Streamer[(K, V)], right: Streamer[(K, V)])
        : Streamer[(K, (Iterable[V], Iterable[V]))] =
      right.tipe match {
        case StreamerType.ViewBased =>
            _coGroup(joinType)(
                keyGrouping(left .selfClosingIterator),
                keyGrouping(right.selfClosingIterator))
              .pipe(new ViewStreamer(_))

        // ---------------------------------------------------------------------------
        case StreamerType.IteratorBased | StreamerType.RDDBased => // delegate
          left
            .pipe(right.toMeBased)
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
                keyGrouping(left .selfClosingIterator),
                keyGrouping(right.selfClosingIterator))
            .pipe(new ViewStreamer(_))

        // ---------------------------------------------------------------------------
        case StreamerType.IteratorBased | StreamerType.RDDBased => ??? // delegate (t210726094742)
      }

  // ===========================================================================
  private def _coGroup[K, V]
          (joinType: JoinType)
          ( leftLookup: Map[K, Iterable[V]],
           rightLookup: Map[K, Iterable[V]])
        : ViewRepr[(K, (Iterable[V], Iterable[V]))] =
      _joinValues(joinType)(
           leftLookup.keys.toSeq.distinct,
          rightLookup.keys.toSeq.distinct)
        .toSeq.view
        .map { joinValueOpt =>
          ( joinValueOpt,
             leftLookup.get(joinValueOpt).getOrElse(Nil) ->
            rightLookup.get(joinValueOpt).getOrElse(Nil)) }

  // ===========================================================================
  private def _join[K, V]
          (joinType: JoinType, combiner: (V, V) => V)
          ( leftLookup: Map[K, Iterable[V]],
           rightLookup: Map[K, Iterable[V]])
        : ViewRepr[V] =
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
