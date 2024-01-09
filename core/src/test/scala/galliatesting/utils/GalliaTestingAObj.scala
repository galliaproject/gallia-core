package galliatesting
package utils

// ===========================================================================
object GalliaTestingAObj extends GalliaTestingAObj

// ---------------------------------------------------------------------------
trait GalliaTestingAObj {
  import aptus.{Seq_, String_}
  import gallia.AObj
  import GalliaTestingObj._

  // ===========================================================================
  implicit class GalliaTestingAObj_(aobj: AObj) {

      // ---------------------------------------------------------------------------
      def _assert2(expected: AObj) = {
        assert(
          exactlyEquals(expected),
          Seq(
              expected.formatSandbox.sectionAllOff("expected:"),
              aobj    .formatSandbox.sectionAllOff("actual:"))
            .section) }

      // ---------------------------------------------------------------------------
      def _assert2(origin: AObj, expected: AObj) = {
        assert(
          aobj.c == expected.c,
          Seq(
              expected.c.formatDefault.sectionAllOff("expected:"),
              aobj    .c.formatDefault.sectionAllOff("actual:"),
              origin  .c.formatDefault.sectionAllOff("origin:"))
            .section )

        // ---------------------------------------------------------------------------
        assert(
          aobj.o.debugObj() == expected.o.debugObj(),
          Seq(
              expected.o.debugObj().formatDefault.sectionAllOff("expected:"),
              aobj    .o.debugObj().formatDefault.sectionAllOff("actual:"),
              origin  .o.debugObj().formatDefault.sectionAllOff("origin:"))
            .section)

        // ---------------------------------------------------------------------------
        assert(
          aobj.o == expected.o,
          Seq(
              expected.o.formatDefault.sectionAllOff("expected:"),
              aobj    .o.formatDefault.sectionAllOff("actual:"),
              origin  .o.formatDefault.sectionAllOff("origin:"))
            .section)
      }

      // ===========================================================================
      def exactlyEquals(that: AObj): Boolean =
        aobj.c            == that.c &&
        aobj.o.exactlyEquals(that.o)

      // ---------------------------------------------------------------------------
      def formatSandbox =
        Seq(
          aobj.c           .formatDefault,
          aobj.o           .formatDefault,
          aobj.o.debugObj().formatDefault)
        .joinln
  }

}

// ===========================================================================