package gallia
package data.single

// ===========================================================================
object ObjUtils {

  def combine(rightJoinKey: Key)(o1: Option[Obj], o2: Option[Obj]): Option[Obj] =
    (o1, o2) match {
      case (None   , None   ) => None
      case (Some(x), None   ) => Some(x)
      case (None   , Some(y)) => Some(y)
      case (Some(x), Some(y)) => Some(x.merge(y.removeOpt(rightJoinKey).getOrElse(y))) }

}

// ===========================================================================
