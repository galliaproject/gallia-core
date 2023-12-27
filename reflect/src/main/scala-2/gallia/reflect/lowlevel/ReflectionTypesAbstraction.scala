package gallia
package reflect
package lowlevel

// ===========================================================================
/** differs based on 2.x vs 3.x */
trait ReflectionTypesAbstraction {
  private[gallia] type  WTT[T] = runiverse.WeakTypeTag[T] // TODO: rename
  private[gallia] type CWTT[T] = runiverse.WeakTypeTag[T] // to be phased out
  private[gallia] type CT  [T] = scala.reflect.ClassTag[T] }

// ===========================================================================
