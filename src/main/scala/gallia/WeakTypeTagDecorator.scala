package gallia

// ===========================================================================
private class WeakTypeTagDecorator[T](wtt: WTT[T]) {
  private val fullName: String = reflect.ReflectUtils.fullNameFromType(wtt.tpe)

  // ---------------------------------------------------------------------------
  def sameType(value: Any): Boolean = // TODO: t220411094433 - hopefully there's a cleaner way...
    fullName == reflect.ReflectUtils.fullNameFromValue(value)

  // ---------------------------------------------------------------------------
  // see t210125111338 (union types)
  def ifApplicable(f: T => Any): AnyValue => AnyValue = value =>
    if (sameType(value)) f(value.asInstanceOf[T])
    else                   value
}

// ===========================================================================