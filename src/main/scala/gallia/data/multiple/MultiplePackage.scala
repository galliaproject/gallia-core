package gallia
package data

// ===========================================================================
package object multiple {
  type Streamer[A] = streamer.Streamer[A]

  // ---------------------------------------------------------------------------
  val CantMixIteratorAndRddProcessing = "220721100032 - can't mix iterator and RDD processing"
}

// ===========================================================================
