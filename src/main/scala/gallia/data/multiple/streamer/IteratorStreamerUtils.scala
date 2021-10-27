package gallia
package data.multiple.streamer

import scala.reflect.{ClassTag => CT}

import aptus._
import aptus.aptutils.IteratorUtils

import spilling.SpillingHackSerialization._
import spilling.SpillingHackDeserialization._
import spilling.GnuSortByFirstFieldHack
import spilling.GnuJoinByFirstFieldHack

import heads.merging.MergingData._

// ===========================================================================
object IteratorStreamerUtils {

  private[streamer] def union[B: CT](dis: IteratorStreamer[B], that: Streamer[B]): Iterator[B] =
    that.tipe match {  
      case StreamerType.ViewBased =>
        val left : Streamer[B] = dis .iterator.pipe(Streamer.fromIterator(_))
        val right: Streamer[B] = that.asInstanceOf[IteratorStreamer[B]]
        
        left.iterator ++ right.iterator
        
      // ---------------------------------------------------------------------------
      case StreamerType.IteratorBased =>
        val left : Streamer[B] = dis .asInstanceOf[IteratorStreamer[B]]
        val right: Streamer[B] = that.asInstanceOf[IteratorStreamer[B]]
        
        left.iterator ++ right.iterator

      // ---------------------------------------------------------------------------
      case StreamerType.RDDBased => ??? // TODO
    }

  // =========================================================================== 
  private[streamer] def groupByKey[K: CT, V: CT](itr: Iterator[(K, V)]): Iterator[(K, List[V])] = { // spilling hack            
      // typically groupXN (as opposed to groupX1)
      //   for now can't do the same for key because of Option (see https://github.com/galliaproject/gallia-docs/blob/init/tasks.md#t210116153713)
      val valueIsObj: Boolean = scala.reflect.classTag[V].runtimeClass == classOf[Obj]

      // ---------------------------------------------------------------------------
      // see https://github.com/galliaproject/gallia-core#poor-mans-scaling-spilling
      itr
        .map(serializeSortingLine(valueIsObj))
          .pipe(GnuSortByFirstFieldHack(Hacks.ExecutionContext, debug = "gbk")(numerical = false /* TODO: allow */))
        .map(deserializeSortingLine[K, V](valueIsObj))
          .pipe(IteratorUtils.groupByPreSortedKey)          
    }

  // ===========================================================================
  private[streamer] def join[K: CT, V: CT]
          (joinType: JoinType, combine: (V, V) => V)
          (left: Streamer[(K, V)], right: Streamer[(K, V)])
        : Iterator[V] =
      right.tipe match {
        case StreamerType.ViewBased =>
          if (joinType.isInner || joinType.isLeft) hashJoin(Hacks.LoseOrderOnGrouping)(joinType, combine)(left, right).iterator // TODO: also do right version at least          
          else                                     spillingJoin                              (joinType, combine)(left, right)
  
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

          val lookup: Map[K, Seq[V]] = if (loseRightOrder) right.toList.groupByKey//WithListMap
            else right.toList.groupByKeyWithListMap
  
          left
            .flatMap { case (k, v) =>
              lookup
                .get(k).toSeq.flatten
                .map(combine(v, _)) }
    }
    
    // ---------------------------------------------------------------------------
    private def spillingJoin[K, V]
          (joinType: JoinType, combine: (V, V) => V)
          (left: Streamer[(K, V)], right: Streamer[(K, V)])
        : Iterator[V] = { // spilling hack
      def sortByKey(debug: String)(input: Iterator[(K, V)]): Iterator[Line] =
        input
          .map(serializeSideSortingLine)
            .pipe(GnuSortByFirstFieldHack(Hacks.ExecutionContext, debug)(numerical = false /* TODO: allow */))
  
      // ---------------------------------------------------------------------------              
      val sortedLeft : Iterator[Line] = sortByKey(debug = "left")(left .iterator)
      val sortedRight: Iterator[Line] = sortByKey(debug = "rite")(right.iterator)
  
      // ---------------------------------------------------------------------------
      GnuJoinByFirstFieldHack(Hacks.ExecutionContext)(sortedLeft, sortedRight)
        .map(deserializeJoiningLine[V])
        .map(combine.tupled)               
    }

}

// ===========================================================================
