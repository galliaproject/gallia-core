package gallia
package meta

import aptus.{Anything_, String_, Seq_}
import aptus.DebugString

import data.json.JsonParsing

// ===========================================================================
object MetaObj { // 201222111332

  private val _fields   = "fields"
    private val _key      = "key"
    private val _info     = "info"
      private val _optional = "optional"
      private val _union    = "union"
        private val _multiple  = "multiple"
        private val _valueType = "valueType"

  // ===========================================================================
  def clsFromFile(schemaFilePath: String): Cls =
    schemaFilePath
      .readFileContent()
      .pipe(JsonParsing.parseObject) // TODO: support more than just JSON
      .pipe(cls)

  // ---------------------------------------------------------------------------
  def clsFromString(value: String): Cls =
    value
      .pipe(JsonParsing.parseObject) // TODO: support more than just JSON
      .pipe(cls)

  // ===========================================================================
  def cls(value: Obj): Cls = Cls(value.objs(_fields).map(fld))
  def cls(value: Cls): Obj = obj(_fields -> value.fields.map(fld))

    // ---------------------------------------------------------------------------
    def fld(value: Obj): Fld = Fld(
      key  = value.string(_key).symbol,
      info = value.obj(_info).pipe(info))

    // ---------------------------------------------------------------------------
    def fld(value: Fld): Obj = obj(
        _key  -> value.key.name,
        _info -> value.info.pipe(info))

      // ---------------------------------------------------------------------------
      def info(value: Obj): Info = Info(
          optional = value.boolean(_optional),
          union    = value.objs(_union).map(subInfo))

        // ---------------------------------------------------------------------------
        def info(value: Info): Obj = obj(
            _optional -> value.optional,
            _union    -> value.union.map(subInfo))

          // ---------------------------------------------------------------------------
          def subInfo(value: Obj): SubInfo = SubInfo(
                 value.boolean(_multiple),
                (value.force (_valueType).pipe(valueType)))

              // ---------------------------------------------------------------------------
              def subInfo(value: SubInfo): Obj = obj(
                _multiple  -> value.multiple,
                _valueType -> value.valueType.pipe(valueType))

            // ---------------------------------------------------------------------------
            private def valueType(value: Any) = (value match { // see 210118133408
                case s: String => BasicType.withName(s)
                case o: Obj    =>
                  o.string_(_type) match {
                    case Some("_Enm") => enm(o)
                    case None         => cls(o) } })

            // ---------------------------------------------------------------------------
            private def valueType(value: ValueType): Any = (value match {
                case e      : BasicType._Enm => enm(e)
                case bsc    : BasicType      => bsc.entryName
                case nesting: Cls            => cls(nesting) })

              // ---------------------------------------------------------------------------
              private def enm(value: Obj): BasicType =
                value
                  .strings("values")
                  .map(EnumValue.apply)
                  .pipe(BasicType._Enm.apply)

              // ---------------------------------------------------------------------------
              private def enm(value: BasicType._Enm): Obj =
                obj(
                  _type    -> "_Enm",
                  "values" -> value.stringValues.assert(_.nonEmpty))

  // ===========================================================================
  def formatClassDebug(value: Cls): DebugString = value.fields.map(formatFieldDebug).joinln

    // ---------------------------------------------------------------------------
    def formatFieldDebug(value: Fld): DebugString =
        value
          .key
          .name
          .padRight(16, ' ')
          .tab(value.info.pipe(formatInfoDebug))

      // ---------------------------------------------------------------------------
      def formatInfoDebug(value: Info): DebugString = {
          if(value.isRequired)                              value.union.map(formatSubInfoDebug).join("|")
          else                 s"${formatOptional(true)}\t${value.union.map(formatSubInfoDebug).join("|")}" }

        // ---------------------------------------------------------------------------
        def formatSubInfoDebug(value: SubInfo): DebugString =
            if (value.isSingle)                              formatValueTypeDebug(value.valueType)
            else                s"${formatMultiple(true)}\t${formatValueTypeDebug(value.valueType)}"

          // ---------------------------------------------------------------------------
          def formatValueTypeDebug(value: ValueType): DebugString =
              value match {
                case tipe   : BasicType => tipe.entryName
                case nesting: Cls       => formatClassDebug(nesting).sectionAllOff }
}

// ===========================================================================
