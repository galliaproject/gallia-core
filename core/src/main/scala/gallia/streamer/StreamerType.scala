package gallia
package streamer

import enumeratum.{Enum, EnumEntry}

// ===========================================================================
sealed trait StreamerType extends EnumEntry {
    def isViewBased    : Boolean = this == StreamerType.ViewBased
    def isIteratorBased: Boolean = this == StreamerType.IteratorBased
    def isRDDBased     : Boolean = this == StreamerType.RDDBased
  }

  // ===========================================================================
  object StreamerType extends Enum[StreamerType] {
    val values = findValues

    // ---------------------------------------------------------------------------
    case object ViewBased     extends StreamerType
    case object IteratorBased extends StreamerType
    case object RDDBased      extends StreamerType

    // ---------------------------------------------------------------------------
    /*
      match {
        case StreamerType.ViewBased     => ???
        case StreamerType.IteratorBased => ???
        case StreamerType.RDDBased      => ???
     }
    */

    // ---------------------------------------------------------------------------
    def spillingTriplet(left: Objs, right: Objs): (Boolean /* use spilling? */, (Objs, Objs)) =
      (left.values.tipe, right.values.tipe) match {
        case (ViewBased,     ViewBased    ) => (false, left                  -> right)
        case (ViewBased,     RDDBased     ) => (false, left._toRddBased      -> right)
        case (ViewBased,     IteratorBased) => (true,  left._toIteratorBased -> right)

        case (IteratorBased, ViewBased )    => (true,  left                  -> right._toIteratorBased)
        case (IteratorBased, IteratorBased) => (true,  left                  -> right)
        case (IteratorBased, RDDBased     ) => aptus.illegalState(data.multiple.CantMixIteratorAndRddProcessing)

        case (RDDBased,      ViewBased    ) => (false, left                  -> right._toRddBased)
        case (RDDBased,      RDDBased     ) => (false, left                  -> right)
        case (RDDBased,      IteratorBased) => aptus.illegalState(data.multiple.CantMixIteratorAndRddProcessing) }
  }

// ===========================================================================
