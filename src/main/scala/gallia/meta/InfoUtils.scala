package gallia
package meta

import aptus.String_

import reflect.TypeLeaf

// ===========================================================================
private[gallia] object InfoUtils {

  def forceNestedClass(leaf: TypeLeaf): Cls =
    leaf
      .fields
      .map { field =>
        Fld(
          field.key.symbol,
          field.node.forceNonBObjInfo)
          .setEnumName(field.node.leaf.name) /* mostly for macros */ }
      .pipe(Cls.apply)
      .setName(leaf.name.splitBy(".").last /* TODO: see t210325105833 - need to be in scope for macros */) // mostly for macros

  // ---------------------------------------------------------------------------
  def forceNonBObjSubInfo(node: TypeNode): SubInfo =
    SubInfo(
      node.containerType.isMultiple, // note: ignores optionality
      valueType(None /* ok if used for comparisons only (see 220506101842) */)(
        node.isContainedDataClass)(
        node.forceValidContainer))

  // ---------------------------------------------------------------------------
  def forceNonBObjSubInfo(enmOpt: _EnmOpt)(node: TypeNode): SubInfo =
    SubInfo(
      node.containerType.isMultiple, // note: ignores optionality
      valueType(enmOpt)(
        node.isContainedDataClass)(
        node.forceValidContainer))

  // ---------------------------------------------------------------------------
  def forceNonBObjInfo(node: TypeNode): Info =
      node
        .forceValidContainer
        .pipe(valueType(None /* TODO? */)(node.isContainedDataClass))
        .pipe(node.containerType.info)

    // ---------------------------------------------------------------------------
    private def valueType(enmOpt: _EnmOpt)(isContainedDataClass: Boolean)(leaf: TypeLeaf): ValueType =
         if (isContainedDataClass) forceNestedClass(leaf)
         else                      valueTypeOpt(enmOpt)(leaf).get
  
      // ---------------------------------------------------------------------------
      private def valueTypeOpt(enmOpt: _EnmOpt)(leaf: TypeLeaf): Option[ValueType] =
               if (leaf.enm)          enmOpt.orElse(Some(BasicType._Enm.Dummy) /* typically for validations, see 220506101842 */)
          else if (leaf.isEnumeratum) Some(BasicType._Enm(leaf.enumeratumEnum))
          else if (leaf.bytes)        Some(BasicType._Binary)
          else                        BasicType.fromFullNameOpt(leaf.name)

}

// ===========================================================================
