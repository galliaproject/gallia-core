package gallia
package plans

// ===========================================================================
object AdjacencyGrouping {

  def apply[T](nodes: Seq[T])(grouperPred: T => Boolean): Seq[Seq[T]] = {    
      val mut = cross.MutList[Seq[T]]()
           
      var idx = 0
      val size = nodes.size
      while (idx < size) {
        val curr = nodes(idx)
        if (grouperPred(curr)) {          
          var cont = true
          var idx2 = idx          
          while (idx2 < size && cont) {
            val curr2 = nodes(idx2)            
            if (grouperPred(curr2)) { idx2 += 1 }
            else { cont = false }
          }

          mut += nodes.slice(idx, until = idx2 /* apparently "until" means exclusive... */).toList
          idx = idx2          
        } else {
          mut += List(curr)
          idx += 1
        }        
      }
      
    mut.toList
  }
}

// ===========================================================================
