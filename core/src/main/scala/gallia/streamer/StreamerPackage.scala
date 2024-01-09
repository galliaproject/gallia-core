package gallia

// ===========================================================================
package object streamer {
  type Closeable = java.io.Closeable

  // ---------------------------------------------------------------------------
  type ViewRepr[T] = cross.SeqView[T] // TODO: t210115103554 - confirm reads entire Seq once first (so can redo as needed)?
}

// ===========================================================================
