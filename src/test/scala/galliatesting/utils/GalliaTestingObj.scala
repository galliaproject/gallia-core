package galliatesting
package utils

// ===========================================================================
object GalliaTestingObj extends GalliaTestingObj

// ---------------------------------------------------------------------------
trait GalliaTestingObj {
  import gallia.Obj

  // ===========================================================================
  implicit class GalliaTestingObj_(o: Obj) {
    def exactlyEquals(that: Obj): Boolean =
      o          == that &&
      o.debugObj == that.debugObj

    // ---------------------------------------------------------------------------
    def debugObj(): Obj = toDebug(o)
  }

  // ===========================================================================
  private def toDebug(o: Obj): Obj =
    o.modifyValuesRecursively { bsc =>
      s"${bsc.getClass.getSimpleName}:|${bsc}|" }

}

// ===========================================================================