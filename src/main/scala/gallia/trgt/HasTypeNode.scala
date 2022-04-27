package gallia
package target

// ===========================================================================
trait HasTypeNode {
    val node: TypeNode

    // ---------------------------------------------------------------------------
    def ofniOpt(validator: TypeNode => Boolean): Option[Ofni] =
      if (validator(node)) Some(node.forceNonBObjOfni)
      else                 None

    // ---------------------------------------------------------------------------
    def _out(value: Any): Any = DataClassUtils._out(node)(value)
  }

  // ===========================================================================
  object HasTypeNode {
    implicit def to(x: TypeNode): HasTypeNode = new HasTypeNode { val node = x }
  }

// ===========================================================================