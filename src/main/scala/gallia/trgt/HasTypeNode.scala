package gallia
package target

// ===========================================================================
trait HasTypeNode {
    val typeNode: TypeNode

    // ---------------------------------------------------------------------------
    def info1Opt(validator: TypeNode => Boolean): Option[Info1] =
      if (validator(typeNode)) Some(typeNode.forceNonBObjInfo.forceInfo1)
      else                 None

    // ---------------------------------------------------------------------------
    /** static -> dynamic */
    def toDynamic(value: Any): Any =
      if (!typeNode.isContainedDataClass) value
      else                                typeNode.forceNonBObjInfo.pipe(StaticToDynamic(value)) }

  // ===========================================================================
  object HasTypeNode {
    implicit def to(x: TypeNode): HasTypeNode = new HasTypeNode { val typeNode = x } }

// ===========================================================================
