package gallia
package target

// ===========================================================================
class HT(
        val node        : TypeNode,
        val instantiator: Instantiator)
      extends HasType // TODO: rename + homogenize use of HasType vs HT

    // ===========================================================================
    object HT {

      def parse[T: WTT]: HT =
        typeNode[T].pipe { node =>
          new HT(node, instantiator[T](node)) }

      // ---------------------------------------------------------------------------
      /**/    def instantiator[T: WTT]              : Instantiator = instantiator[T](typeNode[T]) //TODO:to private
      private def instantiator[T: WTT](to: TypeNode): Instantiator = {
        if (!to.isContainedDataClass) null // TODO
        else {
               if (to.isOptionOfSeq)        low.instantiateFromFirstTypeArgFirstTypeArg[T]       // eg Option[Seq[MyCc]]
          else if (to.isSeq || to.isOption) low.instantiateFromFirstTypeArg            [T]       // eg Option[    MyCc ]
          else                              low.instantiateFromTypeDirectly            [T] } } } // eg            MyCc

  // ===========================================================================
  class HT2(val ht1: HT, val ht2: HT) extends HasTypes2

    // ---------------------------------------------------------------------------
    object HT2 {
      def from[T1: WTT, T2: WTT] =
        new HT2(
          new HT(typeNode[T1], HT.instantiator[T1]),
          new HT(typeNode[T2], HT.instantiator[T2]) ) }

// ===========================================================================
