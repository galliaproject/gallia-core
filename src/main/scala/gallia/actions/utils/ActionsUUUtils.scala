package gallia.actions.utils

import gallia._
import gallia.domain.PathPair
import gallia.atoms.AtomsOthers._Nested
import gallia.atoms.AtomsUUVeryBasics._Rename

// ===========================================================================
private[actions] object ActionsUUUtils {

  def _atoms(f: Key => AtomUU)(values: RPathz): Seq[AtomUU] = _atoms(values.values, f)

    def _atoms(values: RPathz    , f: Key => AtomUU): Seq[AtomUU] = _atoms(values.values, f)
    def _atoms(values: KPathz    , f: Key => AtomUU): Seq[AtomUU] = _atoms(values.qpathz, f)
    def _atoms(values: Seq[RPath], f: Key => AtomUU): Seq[AtomUU] =
      values
        .flatMap { qpath =>
          val (parentOpt, ren) = qpath.initPair

          val roots = potentialRenaming(ren) :+ f(ren.to)

          parentOpt
            .map { parent => roots.map(_Nested(parent, _)) }
            .getOrElse(      roots)
        }

  // ---------------------------------------------------------------------------
  def _atoms(c: Cls)(f: PathPair => AtomUU)(values: Seq[RPath]): Seq[AtomUU] =
    values
      .flatMap { qpath =>
        potentialRenaming(qpath) :+ f(PathPair(qpath.to, c.isOptional(qpath.from))) }

  // ===========================================================================
  /*private */def potentialRenaming(ren : Ren  ): Seq[AtomUU] = ren.actualOpt.map(_Rename).toList
  /*private */def potentialRenaming(path: RPath): Seq[AtomUU] = {
    val (parentOpt, ren) = path.initPair

    ren.actualOpt match {
      case None                 => Nil
      case Some(actualRenaming) =>
        Seq(parentOpt match {
          case None         =>                 _Rename(actualRenaming )
          case Some(parent) => _Nested(parent, _Rename(actualRenaming)) }) }
  }

}

// ===========================================================================
