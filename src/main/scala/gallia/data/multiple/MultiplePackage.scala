package gallia.data

// ===========================================================================
package object multiple
    extends gallia.Aliases
    with    gallia.DomainAliases {

  type Obj = gallia.data.single.Obj

  type Streamer[A] = streamer.Streamer[A]
  val  Streamer    = streamer.Streamer
}

// ===========================================================================
