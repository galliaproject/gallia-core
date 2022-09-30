package gallia
package atoms
package obg9

import aptus._

// ===========================================================================
object Obg9Creators {
  import Obg9Contexts._

  // ---------------------------------------------------------------------------
  def tryRetainContiguous(totalSize: Size, indices: Seq[Index] /* sorted + distinct already */) =
    _tryContiguous(totalSize, indices)
      .map { case (fromInclusive, toInclusive) => 
        RetainContiguousCtx.from(totalSize)(fromInclusive, toInclusive) }

  // ---------------------------------------------------------------------------
  def tryRemoveContiguous(totalSize: Size, indices: Seq[Index] /* sorted + distinct already */) =
    _tryContiguous(totalSize, indices)
      .map { case (fromInclusive, toInclusive) => 
        RemoveContiguousCtx.from(totalSize)(fromInclusive, toInclusive) }                

  // ---------------------------------------------------------------------------  
  private def _tryContiguous(totalSize: Size, indices: Seq[Index] /* sorted + distinct already */) = {
    val fromInclusive = indices.min
    val toInclusive   = indices.max
    val contiguous: Boolean = (toInclusive - fromInclusive) == (indices.size - 1)

    if (contiguous) Some((fromInclusive, toInclusive))
    else            None   
  }
  
  // ===========================================================================
  object NonRecursiveReorderingCreator {
    
    def apply(c: Cls)(f: Seq[SKey] => Seq[SKey]): NonRecursiveReorderingCtx =
      c .skeys
        .zipWithIndex
        .toMap
        .pipe { lookup: Map[SKey, Index] =>
          f(c.skeys).map(lookup.apply) }

  }

  // ===========================================================================
  object RecursiveReorderingCreator {
    def apply(c: Cls)(f: Seq[SKey] => Seq[SKey]): RecursiveReordering = {
        val lookup: Map[SKey, (PPair, Option[Cls])] = this.lookup(c)
  
        f
          .apply(c.skeys)
          .map(lookup.apply)
          .map { (item _).tupled(_)(f) }
          .pipe(RecursiveReordering.apply)
      }
    
      // ---------------------------------------------------------------------------
      private def lookup(c: Cls): Map[SKey, (PPair, Option[Cls])] =
          c .fields
            .zipWithIndex
            .map { case (field, index) =>
              field.skey -> 
                (field.ppair(index), field.nestedClassOpt) }
            .toMap
            
      // ---------------------------------------------------------------------------
      private def item
            (ppair: PPair, nestingOpt: Option[Cls])
            (f: Seq[SKey] => Seq[SKey])
          : RecursiveReorderingItem =
         RecursiveReorderingItem(
             ppair.index,
             nestingOpt.map { nc => RecursiveReorderingNesting(
                 apply(nc)(f), 
                 ppair.optional,
                 ppair.multiple) } )
  }
  
  // ===========================================================================
  object RemoveOrRetainWithNestingCtxCreator {

    def apply(c: Cls, pathz: KPathz /* distinct already */): RemoveOrRetainWithNestingCtx = {        
        // TODO: must address: t220414112604                
        val topLevelIndices = pathz.topLevelsOnly.pipe(c.indices)        
    
        RemoveOrRetainWithNestingCtx(
          newSize         = c.size - topLevelIndices.size, 
          topLevelIndices = topLevelIndices.toSet,
          nesting         = nestingMap(c, pathz))
      }
    
      // ---------------------------------------------------------------------------
      private def nestingMap(c: Cls, pathz: KPathz): Map[Index, RemoveOrRetainWithNestingSubCtx] =        
        nestingTmp(c, pathz)
          .map { case (ppair, (nc, sub)) =>
            ppair.index ->
              RemoveOrRetainWithNestingSubCtx(
                apply(nc, sub), 
                ppair.optional,
                ppair.multiple) }
      
      // ---------------------------------------------------------------------------
      def nestingTmp(c: Cls, pathz: KPathz): Map[PPair, (Cls, KPathz)] =
        pathz
          .distinct
          .values
          .map { path => path.first -> path.tail }
          .groupByKey
          .view
          .mapValues(_.flatten.distinct)
          .filterNot(_._2.isEmpty)
          .map { case (key, subPaths) =>         
            c.ppair(key) -> 
              (c.field(key).forceNestedClass, 
               KPathz(subPaths).distinct /* should already but in case */) }
          .toMap
      
  }

}

// ===========================================================================
