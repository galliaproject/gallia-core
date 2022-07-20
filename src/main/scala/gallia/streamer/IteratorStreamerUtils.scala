package gallia
package streamer

import scala.reflect.{ClassTag => CT}

import aptus._

import spilling.SpillingHackSerialization._
import spilling.SpillingHackDeserialization._
import spilling.GnuSortByFirstFieldsHack
import spilling.GnuJoinByFirstFieldHack

import heads.merging.MergingData._

// ===========================================================================
object IteratorStreamerUtils {

  private[streamer] def union[B: CT](dis: IteratorStreamer[B], that: Streamer[B]): IteratorStreamer[B] =
    that.tipe match {  
      case StreamerType.ViewBased | StreamerType.IteratorBased =>
        new data.DataRegenerationClosure[B] {
            def regenerate = () => dis.closeabledIterator.union(that.closeabledIterator) }
          .pipe(IteratorStreamer.from4)

      // ---------------------------------------------------------------------------
      case StreamerType.RDDBased => ??? /* TODO */ }

  // ===========================================================================
  private[streamer] def zip[B: CT](dis: IteratorStreamer[B], that: Streamer[B], combiner: (B, B) => B): IteratorStreamer[B] =
    that.tipe match {
      case StreamerType.ViewBased | StreamerType.IteratorBased =>
        new data.DataRegenerationClosure[B] {
            def regenerate = () => dis.closeabledIterator.zipSameSize(that.closeabledIterator).map(x => combiner(x._1, x._2)) }
          .pipe(IteratorStreamer.from4)

      // ---------------------------------------------------------------------------
      case StreamerType.RDDBased => ??? /* TODO */ }

  // ===========================================================================
  private[streamer] def join[K: CT, V: CT]
          (joinType: JoinType, combine: (V, V) => V)
          (left: Streamer[(K, V)], right: Streamer[(K, V)])
        : Streamer[V] =
      right.tipe match {
        case StreamerType.ViewBased =>
          if (joinType.isInner || joinType.isLeft) hashJoin(Hacks.LoseOrderOnGrouping)(joinType, combine)(left, right) // TODO: also do right version at least
          else                                     spillingJoin                       (joinType, combine)(left, right)
  
        // ---------------------------------------------------------------------------
        case StreamerType.IteratorBased => spillingJoin(joinType, combine)(left, right)          
        case StreamerType.RDDBased      => ??? // delegate
      }

    // ===========================================================================
    private def hashJoin[K: CT, V: CT]
          (loseRightOrder: Boolean)
          (joinType: JoinType, combine: (V, V) => V)
          (left: Streamer[(K, V)], right: Streamer[(K, V)])
        : Streamer[V] = {
          val lookup: Map[K, Seq[V]] =
            if (loseRightOrder) right.toList.groupByKey
            else                right.toList.groupByKeyWithListMap
  
          left
            .flatMap { case (k, v) =>
              lookup
                .get(k).toSeq.flatten
                .map(combine(v, _)) }
    }
    
    // ---------------------------------------------------------------------------
    // see https://github.com/galliaproject/gallia-core#poor-mans-scaling-spilling
    private def spillingJoin[K, V]
          (joinType: JoinType, combine: (V, V) => V)
          (left: Streamer[(K, V)], right: Streamer[(K, V)])
        : Streamer[V] = { // spilling hack
      def sortByKey(debug: String)(input: CloseabledIterator[(K, V)]): CloseabledIterator[Line] =
        input
          .map(serializeSideSortingLine)
            .pipe(GnuSortByFirstFieldsHack.default(Hacks.ExecutionContext, debug)) // TODO: allow numerical here?
  
      // ---------------------------------------------------------------------------
      val leftItr  = left .closeabledIterator
      val rightItr = right.closeabledIterator

      // ---------------------------------------------------------------------------
      new data.DataRegenerationClosure[V] {
          def regenerate = () => {

            val sortedLeft : CloseabledIterator[Line] = sortByKey(debug = "left")(leftItr )
            val sortedRight: CloseabledIterator[Line] = sortByKey(debug = "rite")(rightItr)

            // ---------------------------------------------------------------------------
            GnuJoinByFirstFieldHack(Hacks.ExecutionContext)(sortedLeft, sortedRight)
              .map(deserializeJoiningLine[V])
              .map(combine.tupled) } }
        .pipe(IteratorStreamer.from4)
    }

}

// ===========================================================================
