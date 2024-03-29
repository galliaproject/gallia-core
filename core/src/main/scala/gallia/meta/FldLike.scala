package gallia
package meta

// ===========================================================================
trait FldLike extends HasKey with InfoLike

  // ---------------------------------------------------------------------------
  trait HasKey {
    val  key: Key
    def skey: SKey = key.name
  }

// ===========================================================================
