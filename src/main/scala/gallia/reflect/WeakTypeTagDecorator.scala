package gallia
package reflect

// ===========================================================================
class WeakTypeTagDecorator[T: WTT]() {
  private val fullName: FullNameString = reflect.low.fullName[T]

  // ---------------------------------------------------------------------------
  // TODO: t220411094433 - hopefully there's a cleaner way...
  private def sameType(value: Any): Boolean = fullName == reflect.FullName.fromRuntimeValue(value)

  // ===========================================================================
  // see t210125111338 (union types)
  def ifApplicable(f: T => Any): AnyValue => AnyValue =
    value =>
      if (sameType(value)) f(value.asInstanceOf[T])
      else                   value }

// ===========================================================================
