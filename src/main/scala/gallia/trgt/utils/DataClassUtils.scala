package gallia.target.utils

import gallia.meta.Info
import gallia.reflect.TypeNode
import gallia.target.Instantiator

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
  private def __in(info: Info, instantiator: Instantiator)(value: Any): Any =
           if (info.isOne) { val c2 = info.forceNestedClass; c2.valueFromObj  (instantiator)(value) }
      else if (info.isOpt) { val c2 = info.forceNestedClass; c2.valueFromObj_ (instantiator)(value) }
      else if (info.isNes) { val c2 = info.forceNestedClass; c2.valueFromObjs (instantiator)(value) }
      else if (info.isPes) { val c2 = info.forceNestedClass; c2.valueFromObjs_(instantiator)(value) }
      else ???  // TODO: as match rather

    // ---------------------------------------------------------------------------
    private def __out(info: Info)(value: Any): Any =
           if (info.isOne) { val c2 = info.forceNestedClass; c2.valueToObj  (value) }
      else if (info.isOpt) { val c2 = info.forceNestedClass; c2.valueToObj_ (value) }
      else if (info.isNes) { val c2 = info.forceNestedClass; c2.valueToObjs (value) }
      else if (info.isPes) { val c2 = info.forceNestedClass; c2.valueToObjs_(value) }
      else ???  // TODO: (match rather)

}

// ===========================================================================