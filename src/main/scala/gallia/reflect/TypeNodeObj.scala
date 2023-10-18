package gallia
package reflect

// ===========================================================================
object TypeNodeObj {

  def typeNode(value: TypeNode): Obj =
        obj(
          "leaf" -> tipe(value.leaf),
          "args" -> value.args.map(typeNode))

    // ---------------------------------------------------------------------------
    def tipe(value: TypeLeaf): Obj =
        obj(
          "name"        -> value.name,
          "inScopeName" -> value.inScopeName,
          "alias"       -> value.alias,

          "dataClass"   -> value.dataClass,
          "enm"         -> value.enm,
          "bytes"       -> value.bytes,
          "inheritsSeq" -> value.inheritsSeq,

          "enumeratumValueNamesOpt" -> value.enumeratumValueNamesOpt,

          "fields"    -> value.fields.map(field))

      // ---------------------------------------------------------------------------
      def field(value: Field): Obj =
        obj(
          "key"  -> value.key,
          "node" -> value.node.pipe(typeNode)) }

// ===========================================================================