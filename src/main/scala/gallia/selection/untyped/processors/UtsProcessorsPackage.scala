package gallia.selection.untyped

import gallia._

// ===========================================================================
package object processors { // TODO: rename...
  type KPaths = Seq[KPath]
  type Keys   = Seq[Key]

  // ---------------------------------------------------------------------------
  /** eg -1 allowed for last */ type MIndex = aptus.MirrorIndex
                                type  Index = aptus.Index
}

// ===========================================================================
