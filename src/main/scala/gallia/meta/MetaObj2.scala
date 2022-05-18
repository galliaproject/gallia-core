package gallia
package meta

import aptus.Anything_

// ===========================================================================
object MetaObj2 { // 201222111332
  import MetaKey._

  // ---------------------------------------------------------------------------
  def clsToObj(value: Cls): Obj = obj(_fields -> value.fields.map(fld))

    // ---------------------------------------------------------------------------
    private def fld(value: Fld): Obj = obj(
        _key  -> value.key.name,
        _info -> value.info.pipe(info))

        // ---------------------------------------------------------------------------
        private def info(value: Info): Obj = obj(
            _optional -> value.optional,
            _union    -> value.union.map(subInfo))

            // ---------------------------------------------------------------------------
            private def subInfo(value: SubInfo): Obj = obj(
                _multiple  -> value.multiple,
                _valueType -> value.valueType.pipe(valueType))

              // ---------------------------------------------------------------------------
              private def valueType(value: ValueType): Any = (value match {
                  case e      : BasicType._Enm => enm(e)
                  case bsc    : BasicType      => bsc.entryName
                  case nesting: Cls            => clsToObj(nesting) })

                // ---------------------------------------------------------------------------
                private def enm(value: BasicType._Enm): Obj =
                  obj(
                    _type    -> "_Enm",
                    "values" -> value.stringValues.assert(_.nonEmpty))

}

// ===========================================================================
