package gallia.meta

import aptus.{Anything_, String_}
import gallia.reflect.TypeLeaf

// ===========================================================================
private[gallia] object InfoUtils {

  def forceNestedClass(leaf: TypeLeaf): Cls =
     leaf
       .fields
       .map { field => Fld(
           field.key.symbol,
           field.node.forceNonBObjInfo) }
       .thn(Cls.apply)

  // ---------------------------------------------------------------------------
  def forceNonBObjInfo(node: TypeNode): Info =
     Info(
         node.containerType,
         node.forceValidContainer.thn(containee(node.isContainedDataClass)))

  // ---------------------------------------------------------------------------
    def containee(isContainedDataClass: Boolean)(leaf: TypeLeaf): Containee =
       if (isContainedDataClass) forceNestedClass(leaf)
       else                      containeeOpt(leaf).get

    // ---------------------------------------------------------------------------
    def containeeOpt(leaf: TypeLeaf): Option[Containee] =
       if (leaf.enum) BasicType._Enum.as.some
       else           BasicType.fromFullNameOpt(leaf.name)

}

// ===========================================================================
