package gallia.atoms.utils

import gallia._

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
  def nestingx(dis: Obj, path: KPath)
        (f: (Obj, Key  ) => Obj)
        (g: (Obj, KPath) => Obj): Obj =
    path.tailPair match {
      case (leaf  , None      ) => f(dis, leaf)
      case (parent, Some(tail)) => dis.transformObj(parent, y => g(y, tail)) }

}

// ===========================================================================
