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
}

// ===========================================================================
