package gallia
package reflect
package lowlevel

// ===========================================================================
trait ReflectionTypesAbstraction {
  private[gallia]  type WTT[T] = scala.reflect.runtime.universe.WeakTypeTag[T]
  private[gallia]  type CT [T] = scala.reflect.ClassTag[T] }

// ===========================================================================
