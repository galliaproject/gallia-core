package gallia
package target
package utils

import meta.SubInfo
import meta.Info
import reflect.TypeNode
import target.Instantiator

// ===========================================================================
private[target] object DataClassUtils {
  // wouldn't need to recompute info if we had result cls (TODO)

  // ===========================================================================
  def _in(node: TypeNode, instantiator: Instantiator)(value: Any): Any =
      if (!node.isContainedDataClass) value
      else                            __in(node.forceNonBObjInfo, instantiator)(value)

    // ---------------------------------------------------------------------------
    def _out(node: TypeNode)(value: Any): Any =
      if (!node.isContainedDataClass) value
      else                            __out(node.forceNonBObjInfo)(value)

  // ===========================================================================
  private def __in(subInfo: Info, instantiator: Instantiator)(value: Any): Any = {
         if (subInfo.isOne) { val c2 = subInfo.forceNestedClass; c2.valueFromObj  (instantiator)(value) }
    else if (subInfo.isOpt) { val c2 = subInfo.forceNestedClass; c2.valueFromObj_ (instantiator)(value) }
    else if (subInfo.isNes) { val c2 = subInfo.forceNestedClass; c2.valueFromObjs (instantiator)(value) }
    else if (subInfo.isPes) { val c2 = subInfo.forceNestedClass; c2.valueFromObjs_(instantiator)(value) }
    else ??? } // TODO: as match rather

  // ---------------------------------------------------------------------------
  private def __out(subInfo: Info)(value: Any): Any = {
         if (subInfo.isOne) { val c2 = subInfo.forceNestedClass; c2.valueToObj  (value) }
    else if (subInfo.isOpt) { val c2 = subInfo.forceNestedClass; c2.valueToObj_ (value) }
    else if (subInfo.isNes) { val c2 = subInfo.forceNestedClass; c2.valueToObjs (value) }
    else if (subInfo.isPes) { val c2 = subInfo.forceNestedClass; c2.valueToObjs_(value) }
    else ??? } // TODO: (match rather)

}

// ===========================================================================
