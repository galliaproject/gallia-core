package gallia
package actions.utils

import aptus.Tuple2_

import domain.PathPair
import atoms.AtomsOthers._Nested
import atoms.AtomsUUVeryBasics._Rename
import atoms.AtomsUUVeryBasics._Remove

// ===========================================================================
private[actions] object ActionsUtils {

  def _atoms(f: Key => AtomUU)(values: RPathz): Seq[AtomUU] = _atoms(values.values, f)

    def _atoms(value : KPath     , f: Key => AtomUU): Seq[AtomUU] = _atoms(RPath.from(value), f)
    def _atoms(value : RPath     , f: Key => AtomUU): Seq[AtomUU] = _atoms(Seq(value)       , f)
    def _atoms(values: RPathz    , f: Key => AtomUU): Seq[AtomUU] = _atoms(values.values    , f)
    def _atoms(values: KPathz    , f: Key => AtomUU): Seq[AtomUU] = _atoms(values.qpathz    , f)
    def _atoms(values: Seq[RPath], f: Key => AtomUU): Seq[AtomUU] =
      values
        .flatMap { qpath =>
          val (parentOpt, ren) = qpath.initPair

          val roots = potentialRenaming(ren).toSeq :+ f(ren.to)

          parentOpt
            .map { parent => roots.map(_Nested(parent, _)) }
            .getOrElse(      roots)
        }

  // ---------------------------------------------------------------------------
  def _atoms(c: Cls)(f: PathPair => AtomUU)(values: Seq[RPath]): Seq[AtomUU] =
    values
      .flatMap { qpath =>
        potentialRenaming(qpath).toSeq :+ f(PathPair(qpath.to, c.isOptional(qpath.from))) }

  // ===========================================================================
  def potentialRenaming(value: Ren  ): Option[AtomUU] = value.actualOpt.map(_Rename)  
  def potentialRenaming(value: RPath): Option[AtomUU] =
    value
      .initPair
      .mapSecond(_.actualOpt)
        match {
          case (_           , None           ) => None
          case (None        , Some(actualRen)) => Some(                _Rename(actualRen))
          case (Some(parent), Some(actualRen)) => Some(_Nested(parent, _Rename(actualRen))) }

    
  // ===========================================================================
  def remove   (value : KPath)     : Seq[AtomUU] = removeAll(KPathz(Seq(value)))
  def removeAll(values: KPathz)    : Seq[AtomUU] = removeAll(values.values)
  def removeAll(values: Seq[KPath]): Seq[AtomUU] = values.toSeq.map(potentiallyNested(_Remove))      

    // ---------------------------------------------------------------------------
    private def potentiallyNested(nestee: Key => AtomUU)(path: KPath): AtomUU =
      path.initPair match {
        case (None      , leaf) =>                        nestee(leaf)
        case (Some(tail), leaf) => _Nested(parent = tail, nestee(leaf)) }
        
}

// ===========================================================================
