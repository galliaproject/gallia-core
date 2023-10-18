package gallia
package reflect
package lowlevel

// ===========================================================================
trait ReflectionTypesAbstraction {
  private[gallia] trait WTT[T]
  private[gallia] trait CT [T]

  // ---------------------------------------------------------------------------
  private[gallia] object WTT { implicit def _wtt[T]: WTT[T] = new WTT[T] {} }
  private[gallia] object CT  { implicit def _ct [T]: CT [T] = new CT [T] {} } }

// ===========================================================================
