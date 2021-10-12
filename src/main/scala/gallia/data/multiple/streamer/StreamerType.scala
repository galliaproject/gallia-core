package gallia
package data.multiple.streamer

import enumeratum.{Enum, EnumEntry}

// ===========================================================================
sealed trait StreamerType extends EnumEntry

  // ---------------------------------------------------------------------------
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