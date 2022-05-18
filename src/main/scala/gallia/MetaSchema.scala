package gallia

// ===========================================================================
object MetaSchema {

  private def metaSchemaLeaf             : Cls = metaSchema(valueType = _.string /* BasicType name, eg "_Int" */) // TODO: t220518113447 - enum
  private def metaSchemaRec(nesting: Cls): Cls = metaSchema(valueType = _.requiredUnion(meta.SubInfo.string, meta.SubInfo.single(nesting)))

    // ---------------------------------------------------------------------------
    private def metaSchema(valueType: String => Fld): Cls =
      cls(
        "fields".clss(
          "key".string,
          "info".cls(
            "optional".boolean,
            "union"   .clss(
              "multiple".boolean,
              valueType("valueType")))))

  // ===========================================================================
  def apply: Cls = withDepth(3 /* arbitrarily */)

  // ---------------------------------------------------------------------------
  def withDepth(depth: Int): Cls =
    if (depth == 0) metaSchemaLeaf
      else          metaSchemaRec(withDepth(depth - 1))

}

// ===========================================================================
