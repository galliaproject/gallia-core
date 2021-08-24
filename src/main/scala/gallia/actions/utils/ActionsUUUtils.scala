package gallia.actions.utils

import aptus.Tuple2_
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
  /*private */def potentialRenaming(value: Ren  ): Option[AtomUU] = value.actualOpt.map(_Rename)  
  /*private */def potentialRenaming(value: RPath): Option[AtomUU] =
    value
      .initPair
      .mapSecond(_.actualOpt)
        match {
          case (_           , None           ) => None
          case (None        , Some(actualRen)) => Some(                _Rename(actualRen))
          case (Some(parent), Some(actualRen)) => Some(_Nested(parent, _Rename(actualRen))) }

}

// ===========================================================================
