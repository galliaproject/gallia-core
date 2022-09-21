package gallia
package data

// ===========================================================================
package object multiple
    extends       Aliases
    with    DomainAliases { 
  type Streamer[A] = streamer.Streamer[A]

  // ---------------------------------------------------------------------------
  private[multiple] type OObj = Option[Obj]
  private[multiple] type OVle = Option[Vle]

  // ---------------------------------------------------------------------------
  val CantMixIteratorAndRddProcessing = "220721100032 - can't mix iterator and RDD processing"
}

// ===========================================================================
