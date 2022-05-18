package gallia

// ===========================================================================
object MetaSchema {

  private def metaSchemaLeaf             : Cls = metaSchema(containee = _.string /* BasicType name, eg "_Int" */) // TODO: t220518113447 - enum
  private def metaSchemaRec(nesting: Cls): Cls = metaSchema(containee = _.requiredUnion(meta.Info.string, meta.Info.single(nesting)))

    // ---------------------------------------------------------------------------
    private def metaSchema(containee: String => Fld): Cls =
      cls(
        "fields".clss(
          "key".string,
          "ofni".cls(
            "optional".boolean,
            "infos".clss(
              "multiple".boolean,
              containee("containee")))))

  // ===========================================================================
  def apply: Cls = withDepth(3 /* arbitrarily */)

  // ---------------------------------------------------------------------------
  def withDepth(depth: Int): Cls =
    if (depth == 0) metaSchemaLeaf
      else          metaSchemaRec(withDepth(depth - 1))

}

// ===========================================================================
