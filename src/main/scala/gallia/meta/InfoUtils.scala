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
      containee(
        node.isContainedDataClass)(
        node.forceValidContainer))

  // ---------------------------------------------------------------------------
  def forceNonBObjOfni(node: TypeNode): Ofni =
      node
        .forceValidContainer
        .pipe(containee(node.isContainedDataClass))
        .pipe(node.containerType.ofni)

    // ---------------------------------------------------------------------------
    private def containee(isContainedDataClass: Boolean)(leaf: TypeLeaf): Containee =
         if (isContainedDataClass) forceNestedClass(leaf)
         else                      containeeOpt(leaf).get
  
      // ---------------------------------------------------------------------------
      def containeeOpt(leaf: TypeLeaf): Option[Containee] =
              if (leaf.enm)   Some(BasicType._Enum )
         else if (leaf.bytes) Some(BasicType._Binary)
         else                 BasicType.fromFullNameOpt(leaf.name)

}

// ===========================================================================
