package gallia.data.multiple.streamer

import scala.util.chaining._
import scala.reflect.{ClassTag => CT}

import aptus._
import aptus.utils.IteratorUtils

import spilling.SpillingHackSerialization._
import spilling.SpillingHackDeserialization._
import spilling.GnuSortByFirstFieldHack
import spilling.GnuJoinByFirstFieldHack

import gallia.heads.merging.MergingData._

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
  private[streamer] def groupByKey[K: CT, V: CT](itr: Iterator[(K, V)]): Iterator[(K, List[V])] = { // hack            
      // typically groupXN (as opposed to groupX1)
      //   for now can't do the same for key because of Option (see https://github.com/galliaproject/gallia-docs/blob/init/tasks.md#t210116153713)
      val valueIsObj: Boolean = scala.reflect.classTag[V].runtimeClass == classOf[gallia.data.single.Obj]

      // ---------------------------------------------------------------------------
      // see https://github.com/galliaproject/gallia-core#poor-mans-scaling-spilling
      itr
        .map(serializeSortingLine(valueIsObj))
          .pipe(GnuSortByFirstFieldHack(spilling.executionContext, debug = "gbk")(numerical = false /* TODO: allow */))
        .map(deserializeSortingLine[K, V](valueIsObj))
          .pipe(IteratorUtils.groupByPreSortedKey)          
    }

  // ===========================================================================
  private[streamer] def join[K: CT, V: CT]
        (joinType: JoinType, combine: (V, V) => V)
        (left: Streamer[(K, V)], right: Streamer[(K, V)])
      : Iterator[V] =
    right.tipe match {

      // ---------------------------------------------------------------------------
      case StreamerType.ViewBased | StreamerType.IteratorBased =>
        
        def sortByKey(debug: String)(input: Iterator[(K, V)]): Iterator[Line] =
          input
            .map(serializeSideSortingLine)
              .pipe(GnuSortByFirstFieldHack(spilling.executionContext, debug)(numerical = false /* TODO: allow */))

        // ---------------------------------------------------------------------------              
        val sortedLeft : Iterator[Line] = sortByKey(debug = "left")(left .iterator)
        val sortedRight: Iterator[Line] = sortByKey(debug = "rite")(right.iterator)

        // ---------------------------------------------------------------------------
        GnuJoinByFirstFieldHack(spilling.executionContext)(sortedLeft, sortedRight)
          .map(deserializeJoiningLine[V])
          .map(combine.tupled)               

      // ---------------------------------------------------------------------------         
      case StreamerType.RDDBased => ??? // delegate
    }

}

// ===========================================================================
