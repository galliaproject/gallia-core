package gallia.data.multiple.streamer

import scala.util.chaining._
import scala.reflect.{ClassTag => CT}

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
        .map(spilling.SpillingHackUtils.serializeEntry(valueIsObj))
          .pipe(spilling.GnuSortByFirstFieldHack(numerical = false /* TODO: allow */))
        .map(spilling.SpillingHackUtils.deserializeEntry[K, V](valueIsObj))
          .pipe(groupByPreSortedKey)  
    }

    // ===========================================================================      
    private def groupByPreSortedKey[K, V](itr: Iterator[(K, V)]): Iterator[(K, List[V])] = {
      var previousKeyOpt: Option[K] = None
      
      var currentGroup = cross.MutList[V]()
      
      itr
        .flatMap { current =>          
          val (currentKey, currentValue) = (current._1, current._2)
  
          val tmp =
            if (previousKeyOpt.exists(_ != currentKey)) { // new key
              val entry = previousKeyOpt.get -> currentGroup.toList
              
              currentGroup = cross.MutList[V]()
             
              Some(entry)             
            }
            else // repeated key
              None
          
          currentGroup += currentValue            
          previousKeyOpt = Some(currentKey)
          
          tmp } ++
        previousKeyOpt.map(_ -> cross.mutList(currentGroup)).iterator
    }

}

// ===========================================================================
