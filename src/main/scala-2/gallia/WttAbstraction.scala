package gallia

// ===========================================================================
trait WttAbstraction {
  private[gallia] type UType  = scala.reflect.api.Universe#Type
  private[gallia] type WTT[T] = scala.reflect.runtime.universe.WeakTypeTag[T]
  private[gallia] type CT [T] = scala.reflect.ClassTag[T] }

// ===========================================================================
