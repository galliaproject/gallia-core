package gallia.atoms.obg9

// ===========================================================================
object Obg9Contexts {

  case class Range(start1: Index, start2: Index, length: Size) {
    def copy(ori: Array[Any], dest: Array[Any]) = {
      if (length > 0) // TODO: worth it?
        java.lang.System.arraycopy(ori, start1, dest, start2, length) } }

  // ===========================================================================
  case class RemoveContiguousCtx(left: Range, right: Range, newSize: Size) {
      def doubleCopy(ori: Array[Any], dest: Array[Any]): Unit = {
        left .copy(ori, dest)
        right.copy(ori, dest) } }

    // ---------------------------------------------------------------------------
    object RemoveContiguousCtx {

      def from(totalSize: Size)(fromInclusive: Index, toInclusive: Index): RemoveContiguousCtx = {
        val diff = toInclusive - fromInclusive
        require(diff >= 0, toInclusive -> fromInclusive)

        val toExclusive = toInclusive + 1
        RemoveContiguousCtx(
          left   = Range(          0,             0, length = fromInclusive),
          right  = Range(toExclusive, fromInclusive, length = totalSize - toExclusive),
          newSize = totalSize - (diff + 1))
      }
    }

  // ===========================================================================
  case class RetainContiguousCtx(foo: Range, newSize: Size) {
      @inline def singleCopy(ori: Array[Any], dest: Array[Any]) = foo.copy(ori, dest)
    }

    // ---------------------------------------------------------------------------
    object RetainContiguousCtx {

      def from(totalSize: Size)(fromInclusive: Index, toInclusive: Index): RetainContiguousCtx = {
        val diff = toInclusive - fromInclusive
        require(diff >= 0, toInclusive -> fromInclusive)

        RetainContiguousCtx(
          foo = Range(fromInclusive, 0, length = diff + 1),
          newSize = diff + 1)           
      }
    }

}

// ===========================================================================
