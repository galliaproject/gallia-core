package gallia
package reflect
package lowlevel

// ===========================================================================
/** differs based on 2.x vs 3.x */
trait ReflectionTypesAbstraction {
  /** Formerly stood for WeakTypeTag, now has a life of its own */
  private[gallia] type WTT[T] = runiverse.WeakTypeTag[T]
  private[gallia] type CT [T] = scala.reflect.ClassTag[T] }

// ===========================================================================
