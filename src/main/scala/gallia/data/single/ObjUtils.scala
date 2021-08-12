package gallia.data.single

// ===========================================================================
object ObjUtils {

  def combine(o1: Option[Obj], o2: Option[Obj]): Option[Obj] =
    (o1, o2) match {
      case (None   , None   ) => None
      case (Some(x), None   ) => Some(x)
      case (None   , Some(y)) => Some(y)
      case (Some(x), Some(y)) => Some(x.merge(y)) }

  // ===========================================================================
  def checkSameTypes(from: Any, to: Any): Unit = {
    val expected = from.getClass().getSimpleName
    val actual   = to  .getClass().getSimpleName

    if (actual != expected)
      gallia.vldt._Error.Runtime.DifferingRuntimeType(expected, actual).throwDataError() // TODO: throw a lower level exception here    
  }
  
}

// ===========================================================================
