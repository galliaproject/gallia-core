package gallia
package target

// ===========================================================================
trait HasTypeNode {
    val node: TypeNode

    // ---------------------------------------------------------------------------
    def info1Opt(validator: TypeNode => Boolean): Option[Info1] =
      if (validator(node)) Some(node.forceNonBObjInfo.forceInfo1)
      else                 None

    // ---------------------------------------------------------------------------
    def _out(value: Any): Any =
      if (!node.isContainedDataClass) value
      else                            node.forceNonBObjInfo.pipe(InstantiatorUtils.out(value)) }

  // ===========================================================================
  object HasTypeNode {
    implicit def to(x: TypeNode): HasTypeNode = new HasTypeNode { val node = x } }

// ===========================================================================
