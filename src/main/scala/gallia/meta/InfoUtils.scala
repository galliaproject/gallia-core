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
          field.node.forceNonBObjOfni)
          .setEnumName(field.node.leaf.name) /* mostly for macros */ }
      .pipe(Cls.apply)
      .setName(leaf.name.splitBy(".").last /* TODO: see t210325105833 - need to be in scope for macros */) // mostly for macros

  // ---------------------------------------------------------------------------
  def forceNonBObjInfo(node: TypeNode): Info =
    Info(
      node.containerType.isMultiple, // note: ignores optionality
      containee(None /* ok if used for comparisons only (see 220506101842) */)(
        node.isContainedDataClass)(
        node.forceValidContainer))

  // ---------------------------------------------------------------------------
  def forceNonBObjInfo(enmOpt: _EnmOpt)(node: TypeNode): Info =
    Info(
      node.containerType.isMultiple, // note: ignores optionality
      containee(enmOpt)(
        node.isContainedDataClass)(
        node.forceValidContainer))

  // ---------------------------------------------------------------------------
  def forceNonBObjOfni(node: TypeNode): Ofni =
      node
        .forceValidContainer
        .pipe(containee(None /* TODO? */)(node.isContainedDataClass))
        .pipe(node.containerType.ofni)

    // ---------------------------------------------------------------------------
    private def containee(enmOpt: _EnmOpt)(isContainedDataClass: Boolean)(leaf: TypeLeaf): Containee =
         if (isContainedDataClass) forceNestedClass(leaf)
         else                      containeeOpt(enmOpt)(leaf).get
  
      // ---------------------------------------------------------------------------
      private def containeeOpt(enmOpt: _EnmOpt)(leaf: TypeLeaf): Option[Containee] =
               if (leaf.enm)          enmOpt.orElse(Some(BasicType._Enm.Dummy) /* typically for validations, see 220506101842 */)
          else if (leaf.isEnumeratum) Some(BasicType._Enm(leaf.enumeratumEnum))
          else if (leaf.bytes)        Some(BasicType._Binary)
          else                        BasicType.fromFullNameOpt(leaf.name)

}

// ===========================================================================
