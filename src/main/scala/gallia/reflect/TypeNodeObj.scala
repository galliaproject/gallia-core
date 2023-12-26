package gallia
package reflect

// ===========================================================================
object TypeNodeObj {

  def typeNode(value: TypeNode): Obj =
        obj(
          "leaf" -> typeLeaf(value.leaf),
          "args" -> value.args.map(typeNode))

    // ---------------------------------------------------------------------------
    def typeLeaf(value: TypeLeaf): Obj =
        obj(
          "name"            -> value.name,

          "dataClass"       -> value.dataClass,
          "galliaEnumValue" -> value.galliaEnumValue,
          "bytes"           -> value.bytes,
          "inheritsSeq"     -> value.inheritsSeq,

          "enumeratumValueNamesOpt" -> value.enumeratumValueNamesOpt,

          "fields" -> value.fields.map(field))

      // ---------------------------------------------------------------------------
      def field(value: Field): Obj =
        obj(
          "key"  -> value.key,
          "node" -> value.typeNode.pype(typeNode)) }

// ===========================================================================