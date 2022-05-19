package gallia
package plans

import atoms.common.AtomsCommonVeryBasics    ._RenameAll
import atoms.common.AtomsCommonSomewhatBasics._RemoveWhateverIfAll
import atoms                                 ._UWrappers

// ===========================================================================
case class AtomNodes(values: Seq[AtomNode]) extends AnyVal {  
        
  def pruneChain: AtomNodes =
    this
      .combine(_.isUWrapper)        (_.asUWrapper)        (_UWrappers          .from)
      .combine(_.isRename)          (_.asRename)          (_RenameAll          .from)
      .combine(_.isRemoveWhateverIf)(_.asRemoveWhateverIf)(_RemoveWhateverIfAll.from) // FIXME: t210727091024 - only if same level        
        
    // ===========================================================================  
    def combine[$Atom <: Atom](pred: Atom => Boolean)(specifier: Atom => $Atom)(combiner: Seq[$Atom] => AtomCombiner[$Atom]): AtomNodes =
      values
          .pipe(AdjacencyGrouping.apply(_)(_.atom.pipe(pred)))
          .pipe(_.flatMap(AtomNodes._combineAtomNodes(_.map(_.atom.pipe(specifier)).pipe(combiner))))
        .pipe(AtomNodes.apply)
}  

// ===========================================================================
object AtomNodes {
  
  private val Threshold = 5 // TODO: better heuristic: as ratio of object size rather  

  // ===========================================================================  
  private def _combineAtomNodes(combiner: Seq[AtomNode] => Atom)(subNodes: Seq[AtomNode]): Seq[AtomNode] =          
      if (subNodes.size <= Threshold)
        subNodes
      else
        Seq(
          subNodes
            .head // MUST use head
            .copy( // 210611124807 - uses temporary hack (moreIds)
                atom    = combiner(subNodes),
                moreIds = subNodes.tail.map(_.id)) )  // TODO: loses exact origin...
                
  // ===========================================================================
  def combineUWrapping(values: Seq[Atom]): Seq[Atom] =
    values
        .pipe(AdjacencyGrouping.apply(_)(_.isRename))
        .pipe(_combineUWrapping(_.map(_.asRename).pipe(_RenameAll.from)))

        .pipe(AdjacencyGrouping.apply(_)(_.isRemoveWhateverIf))
        .pipe(_combineUWrapping(_.map(_.asRemoveWhateverIf).pipe(_RemoveWhateverIfAll.from)))

    // ---------------------------------------------------------------------------      
    private def _combineUWrapping
          (combiner:     Seq[Atom] => Atom)
          (grouped : Seq[Seq[Atom]])
                       : Seq[Atom] =
       grouped
          .flatMap { subNodes =>          
            if (subNodes.size <= Threshold) subNodes          
            else Seq(combiner(subNodes)) }
  
}

// ===========================================================================
