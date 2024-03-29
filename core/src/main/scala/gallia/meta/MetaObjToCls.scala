package gallia
package meta

import aptus.String_
import data.json.GsonParsing

// ===========================================================================
object MetaObjToCls { // 201222111331
  import MetaKey._

  // ---------------------------------------------------------------------------
  def clsFromFile(schemaFilePath: String): Cls =
    schemaFilePath
      .readFileContent()
      .pipe(GsonParsing.parseObject) // TODO: support more than just JSON
      .pipe(clsFromObj)

  // ---------------------------------------------------------------------------
  def clsFromString(value: String): Cls =
    value
      .pipe(GsonParsing.parseObject) // TODO: support more than just JSON
      .pipe(clsFromObj)

  // ---------------------------------------------------------------------------
  def clsFromObj(value: Obj): Cls =
      value
        .objs(_fields)
        .map(fld)
        .pipe(Cls.apply)

  // ===========================================================================
  private def fld(value: Obj): Fld = Fld(
    key  = value.string(_key).symbol,
    info = value.obj(_info).pipe(info))

    // ---------------------------------------------------------------------------
    private def info(value: Obj): Info = Info(
        optional = value.boolean(_optional),
        union    = value.objs(_union).map(subInfo))

      // ---------------------------------------------------------------------------
      private def subInfo(value: Obj): SubInfo = SubInfo(
             value.boolean(_multiple),
            (value.forceKey(Symbol(_valueType)).pipe(valueType)))

        // ---------------------------------------------------------------------------
        private def valueType(value: Any) = (value match { // see 210118133408
            case s: String => BasicType.withName(s)
            case o: Obj    =>
              o.string_(_type) match {
                case Some("_Enm") => enm(o)
                case None         => clsFromObj(o) } })

          // ---------------------------------------------------------------------------
          private def enm(value: Obj): BasicType =
            value
              .strings("values")
              .map (EnumValue.apply)
              .pipe(BasicType._Enm.apply)

}

// ===========================================================================
