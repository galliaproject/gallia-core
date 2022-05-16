package gallia

// ===========================================================================
private class WeakTypeTagDecorator[T](wtt: scala.reflect.runtime.universe.WeakTypeTag[T]) {
  private val fullName: String = wtt.tpe.typeSymbol.fullName

  // ---------------------------------------------------------------------------
  def sameType(value: Any): Boolean = // TODO: t220411094433 - hopefully there's a cleaner way...
    fullName ==
    value.getClass.getName.pipe(reflect.BasicTypeUtils.normalizeFullName)

  // ---------------------------------------------------------------------------
  // see t210125111338 (union types)
  def ifApplicable(f: T => Any): AnyValue => AnyValue = value =>
    if (sameType(value)) f(value.asInstanceOf[T])
    else                   value
}

// ===========================================================================