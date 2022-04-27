package gallia
package meta

import aptus.{Anything_, String_, Seq_}
import aptus.DebugString

import data.json.JsonParsing

// ===========================================================================
object MetaObj { // 201222111332

  private val _fields   = "fields"
    private val _key      = "key"
    private val _ofni     = "ofni"
      private val _optional = "optional"
      private val _infos    = "infos"
        private val _multiple  = "multiple"
        private val _containee = "containee"

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
      ofni = value.obj(_ofni).pipe(ofni))

    // ---------------------------------------------------------------------------
    def fld(value: Fld): Obj = obj(
        _key  -> value.key.name,
        _ofni -> value.ofni.pipe(ofni))

      // ---------------------------------------------------------------------------
      def ofni(value: Obj): Ofni = Ofni(
          optional = value.boolean(_optional),
          infos    = value.objs(_infos).map(info))

        // ---------------------------------------------------------------------------
        def ofni(value: Ofni): Obj = obj(
            _optional -> value.optional,
            _infos    -> value.infos.map(info))

          // ---------------------------------------------------------------------------
          def info(value: Obj): Info = Info(
                 value.boolean(_multiple),
                (value.force (_containee).pipe(containee)))

              // ---------------------------------------------------------------------------
              def info(value: Info): Obj = obj(
                _multiple  -> value.multiple,
                _containee -> value.containee.pipe(containee))

            // ---------------------------------------------------------------------------
            private def containee(value: Any) = (value match { // see 210118133408
              case s: String => BasicType.withName(s)
              case o: Obj    => cls(o) })

            // ---------------------------------------------------------------------------
            private def containee(value: Containee): Any = (value match {
              case tipe   : BasicType => tipe.entryName
              case nesting: Cls       => cls(nesting) })

  // ===========================================================================
  def formatClassDebug(value: Cls): DebugString = value.fields.map(formatFieldDebug).joinln

    // ---------------------------------------------------------------------------
    def formatFieldDebug(value: Fld): DebugString =
        value
          .key
          .name
          .padRight(16, ' ')
          .tab(value.ofni.pipe(formatOfniDebug))

      // ---------------------------------------------------------------------------
      def formatOfniDebug(value: Ofni): DebugString = {
          if(value.isRequired)                              value.infos.map(formatInfoDebug).join("|")
          else                 s"${formatOptional(true)}\t${value.infos.map(formatInfoDebug).join("|")}" }

        // ---------------------------------------------------------------------------
        def formatInfoDebug(value: Info): DebugString =
            if (value.isSingle)                              formatContaineeDebug(value.containee)
            else                s"${formatMultiple(true)}\t${formatContaineeDebug(value.containee)}"

          // ---------------------------------------------------------------------------
          def formatContaineeDebug(value: Containee): DebugString =
              value match {
                case tipe   : BasicType => tipe.entryName
                case nesting: Cls       => formatClassDebug(nesting).sectionAllOff }
}

// ===========================================================================
