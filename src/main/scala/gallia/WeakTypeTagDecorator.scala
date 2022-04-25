package gallia

// ===========================================================================
private class WeakTypeTagDecorator[T](wtt: scala.reflect.runtime.universe.WeakTypeTag[T]) {
  private val name: String = wtt.tpe.typeSymbol.name.decodedName.toString

  // ---------------------------------------------------------------------------
  def sameType(value: Any): Boolean =
    // TODO: t220411094433 - hopefully there's a cleaner way...
    name == value.getClass.getSimpleName

  // ---------------------------------------------------------------------------
  // see t210125111338 (union types)
  def ifApplicable(f: T => Any): AnyValue => AnyValue = value =>
    if (sameType(value)) f(value.asInstanceOf[T])
    else                   value
}

// ===========================================================================