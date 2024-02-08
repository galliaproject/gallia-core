package gallia
package reflect
package lowlevel

// ===========================================================================
/** differs based on 2.x vs 3.x */
trait ReflectionTypesAbstraction {
  /** stands for WeakTypeTag in scala 2.x, has a life of its own in scala 3.x */
  private[gallia] type WTT[T] = runiverse.WeakTypeTag[T]

  // ---------------------------------------------------------------------------
  // don't rely on implicits for Any
  private[gallia] val AnyWTT: WTT[scala.Any] = runiverse.weakTypeTag[scala.Any]

  // ---------------------------------------------------------------------------
  type __WTT_for_testing_only[T] = WTT[T] }

// ===========================================================================
