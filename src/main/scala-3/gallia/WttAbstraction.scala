package gallia

// ===========================================================================
trait WttAbstraction {
  private[gallia] trait UType

  // ---------------------------------------------------------------------------
  private[gallia] trait WTT[T]{ def tpe: UType = ??? }
    object WTT { implicit def _wtt[T]: WTT[T] = new WTT[T] {} }

  // ---------------------------------------------------------------------------
  private[gallia] trait CT[T] {}
    object CT { implicit def _ct[T]: CT[T] = new CT[T] {} }

}

// ===========================================================================
