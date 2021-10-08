package gallia.reflect

import gallia._

// ===========================================================================
object TypeNodeObj {

  def typeNode(value: TypeNode): Obj =
    obj(
        'leaf -> tipe(value.leaf),
        'args -> value.args.map(typeNode))

    // ---------------------------------------------------------------------------
    def tipe(value: TypeLeaf): Obj =
      obj(
        'name      -> value.name,
        'alias     -> value.alias,
        'dataClass -> value.dataClass,
        'fields    -> value.fields.map(field))

      // ---------------------------------------------------------------------------
      def field(value: Field): Obj =
        obj(
          'key -> value.key,
          'node -> value.node.pipe(typeNode))

}

// ===========================================================================