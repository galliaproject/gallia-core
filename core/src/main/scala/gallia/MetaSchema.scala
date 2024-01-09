package gallia

// ===========================================================================
object MetaSchema { import meta.SubInfo

  private def metaSchemaLeaf             : Cls = metaSchema("valueType".string /* BasicType name, eg "_Int" */) // TODO: t220518113447 - enum
  private def metaSchemaRec(nesting: Cls): Cls = metaSchema("valueType".requiredUnion(
    SubInfo.string,
    SubInfo.single(nesting)))

    // ---------------------------------------------------------------------------
    private def metaSchema(valueTypeField: Fld): Cls =
      cls(
        "fields".clss(
          "key".string,
          "info".cls(
            "optional".boolean,
            "union"   .clss(
              "multiple".boolean,
              valueTypeField))))

  // ===========================================================================
  def apply: Cls = withDepth(3 /* arbitrarily */)

  // ---------------------------------------------------------------------------
  def withDepth(depth: Int): Cls =
    if (depth == 0) metaSchemaLeaf
      else          metaSchemaRec(withDepth(depth - 1))

}

// ===========================================================================
