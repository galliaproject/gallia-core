package gallia
package data.multiple.streamer

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
    case object IteratorBased extends StreamerType // relates to t210115104555
    case object RDDBased      extends StreamerType

    // ---------------------------------------------------------------------------
    /*
      match {
        case StreamerType.ViewBased     => ???
        case StreamerType.IteratorBased => ???
        case StreamerType.RDDBased      => ???
     }
    */
  }

// ===========================================================================