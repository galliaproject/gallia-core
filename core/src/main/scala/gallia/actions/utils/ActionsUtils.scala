package gallia
package actions
package utils

import aptus.Tuple2_

import domain.PathPair
import atoms.AtomsOthers._Nested
import atoms.common.AtomsCommonVeryBasics._Rename
import atoms.common.AtomsCommonVeryBasics._Remove

// ===========================================================================
private[actions] object ActionsUtils {

  def _atoms(f: Key => AtomUU)(values: RPathz): Seq[AtomUU] = _atoms(values.values, f)

    def _atoms(value : KPath     , f: Key => AtomUU): Seq[AtomUU] = _atoms(RPath.from(value), f)
    def _atoms(value : RPath     , f: Key => AtomUU): Seq[AtomUU] = _atoms(Seq(value)       , f)
    def _atoms(values: RPathz    , f: Key => AtomUU): Seq[AtomUU] = _atoms(values.values    , f)
    def _atoms(values: KPathz    , f: Key => AtomUU): Seq[AtomUU] = _atoms(values.rpathz    , f)
    def _atoms(values: Seq[RPath], f: Key => AtomUU): Seq[AtomUU] =
      values
        .flatMap { rpath =>
          val (parentOpt, ren) = rpath.initPair

          val roots = potentialRenaming(ren).toSeq :+ f(ren.to)

          parentOpt
            .map { parent => roots.map(_Nested(parent, _)) }
            .getOrElse(      roots)
        }

  // ---------------------------------------------------------------------------
  def _atoms(c: Cls)(f: PathPair => AtomUU)(values: Seq[RPath]): Seq[AtomUU] =
    values
      .flatMap { rpath =>
        potentialRenaming(rpath).toSeq :+ 
        f(PathPair(rpath.to, c.isOptional(rpath.from))) }

  // ---------------------------------------------------------------------------
  def _atomsUnion(c: Cls)(ifOne: PathPair => AtomUU, ifMultiple: PathPair => AtomUU)(values: Seq[RPath]): Seq[AtomUU] = // see t210125111338 (union types)
    values
      .flatMap { rpath =>
        val pair = PathPair(rpath.to, c.isOptional(rpath.from))

        potentialRenaming(rpath).toSeq :+ 
        (if (!c.field(rpath.from).isUnionType) ifOne     (pair) 
         else                                  ifMultiple(pair))}

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
