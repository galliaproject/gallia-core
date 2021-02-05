package gallia.atoms.utils

import aptus.Tuple2_

import gallia._
import gallia.atoms._Nested
import gallia.atoms._Rename

// ===========================================================================
private[atoms] object AtomsUtils {

  def applyx(f: Any => Any)(value: Any): Any =
    value match {
      case None | Nil  => None
      case Some(x)     =>
        x match {
          case seq: Seq[_] => seq.map(f)
          case sgl         => f(sgl) }
      case seq: Seq[_] => seq.map(f)
      case sgl         => f(sgl) }

  // ===========================================================================
  def potentialRenaming(value: RPath): Option[AtomUU] =
    value
      .initPair
      .mapSecond(_.actualOpt)
      match {
        case (_           , None           ) => None
        case (None        , Some(actualRen)) => Some(                _Rename(actualRen))
        case (Some(parent), Some(actualRen)) => Some(_Nested(parent, _Rename(actualRen))) }

  // ===========================================================================
  def nestingx(dis: Obj, path: KPath)
        (f: (Obj, Key  ) => Obj)
        (g: (Obj, KPath) => Obj): Obj =
    path.tailPair match {
      case (leaf  , None      ) =>                          f(dis  , leaf)
      case (parent, Some(tail)) => dis.transformObj(parent, y => g(y, tail)) }

}

// ===========================================================================
