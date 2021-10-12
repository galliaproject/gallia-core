package gallia.target

// ===========================================================================
trait HasTypeNode {
    val node: TypeNode

    // ---------------------------------------------------------------------------
    def infoOpt(validator: TypeNode => Boolean): Option[Info] =
      if (validator(node)) Some(node.forceNonBObjInfo)
      else                 None

    // ---------------------------------------------------------------------------
    def _out(value: Any): Any = DataClassUtils._out(node)(value)
  }

  // ===========================================================================
  object HasTypeNode {
    implicit def to(x: TypeNode): HasTypeNode = new HasTypeNode { val node = x }
  }

// ===========================================================================