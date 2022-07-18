package gallia
package data
package multiple

// ===========================================================================
package object streamer {
  type Closeable = java.io.Closeable

  // ---------------------------------------------------------------------------
  type SuperMetaPair[T] = gallia.atoms.utils.SuperMetaPair[T]
  val  SuperMetaPair    = gallia.atoms.utils.SuperMetaPair

  // ---------------------------------------------------------------------------
  type ViewRepr[T] = cross.SeqView[T] // TODO: t210115103554 - confirm reads entire Seq once first (so can redo as needed)?
}

// ===========================================================================