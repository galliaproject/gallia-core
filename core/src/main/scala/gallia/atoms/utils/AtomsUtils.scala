package gallia
package atoms
package utils

// ===========================================================================
private[atoms] object AtomsUtils {

  def applyx(f: Any => Any)(value: Any): Any =
    value match { // TODO: use schema rather (see t210115095838)
      case None | Nil  => None
      case Some(x)     =>
        x match { // TODO: use schema rather (see t210115095838)
          case seq: Seq[_] => seq.map(f)
          case sgl         => f(sgl) }
      case seq: Seq[_] => seq.map(f)
      case sgl         => f(sgl) }

  // ===========================================================================
  def nestingx(dis: Obj, path: KPath)
        (ifLeaf   : (Obj, Key  ) => Obj)
        (ifNesting: (Obj, KPath) => Obj): Obj =
    path.tailPair match {
      case (leaf  , None      ) => ifLeaf(dis, leaf)
      case (parent, Some(tail)) => dis.transformObjx(parent, ifNesting(_, tail)) }

}

// ===========================================================================
