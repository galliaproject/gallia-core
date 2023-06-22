package galliatesting
package utils

// ===========================================================================
object GalliaTestingHeadO extends GalliaTestingHeadO

// ---------------------------------------------------------------------------
trait GalliaTestingHeadO {
  import gallia.{AObj, BObj, HeadO}
  import GalliaTestingAObj._

  // ===========================================================================
  implicit class GalliaTestingHeadO_(head: HeadO) {
    def _assert2              (expected: BObj) = { head._forceResult._assert2(expected.forceAObj) }
    def _assert2              (expected: AObj) = { head._forceResult._assert2(expected) }
    def _assert2(origin: AObj, expected: AObj) = { head._forceResult._assert2(origin, expected) } }
}

// ===========================================================================