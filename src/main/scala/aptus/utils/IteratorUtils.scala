package aptus.utils

// ===========================================================================
object IteratorUtils {

  def selfClosing[A](parent: Iterator[A], cls: Closeable*): Iterator[A] =
    new Iterator[A] with Closeable {
      private var closed: Boolean = false

      // ===========================================================================
      override def hasNext: Boolean = !closed && parent.hasNext

      override def next(): A = {
        val next = parent.next()
        if (!parent.hasNext) { close() }
        next
      }

      // ---------------------------------------------------------------------------
      override def close() { closed = true; cls.foreach(_.close()) }
    }
  
  // ===========================================================================
  def groupByPreSortedKey[K, V](itr: Iterator[(K, V)]): Iterator[(K, List[V])] = {
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
