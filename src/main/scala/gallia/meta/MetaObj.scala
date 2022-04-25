package gallia
package meta

import aptus.{Anything_, String_, Seq_}
import aptus.DebugString

import data.json.JsonParsing

// ===========================================================================
object MetaObj { // 201222111332

  // TODO: ironically can't create meta schema until/unless support heterogenous types (namely containee as basic value XOR nesting - see t210118133408)

  // ---------------------------------------------------------------------------
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
  def cls(value: Obj): Cls = Cls(value.objs("fields").map(fld))
  def cls(value: Cls): Obj = obj("fields" -> value.fields.map(fld))

    // ---------------------------------------------------------------------------
    def fld(value: Obj): Fld = Fld(value.string("key").symbol, value.objs("infos").map(info))
    def fld(value: Fld): Obj = obj("key" -> value.key.name, "infos" -> value.infos.map(info))

      // ---------------------------------------------------------------------------
      // TODO: check enums ok?

        def info(value: Obj): Info =
          Info.apply(
             value.string("container").pipe(Container.withName),
            (value.force ("containee") match { // see 210118133408
              case s: String => BasicType.withName(s)
              case o: Obj    => cls(o) }))

        // ---------------------------------------------------------------------------
        def info(value: Info): Obj =
          obj(
              "container" -> value.container.entryName,
              "containee" ->
            (value.containee match {
              case tipe   : BasicType => tipe.entryName
              case nesting: Cls       => cls(nesting) }))

  // ===========================================================================
  def formatClassDebug(value: Cls): DebugString = value.fields.map(formatFieldDebug).joinln

    // ---------------------------------------------------------------------------
    def formatFieldDebug(value: Fld): DebugString =
        value
          .key
          .name
          .padRight(16, ' ')
          .tab(value.infos.map(formatInfoDebug).join("|"))

      // ---------------------------------------------------------------------------
      def formatInfoDebug(value: Info): DebugString =
          value.containee match {

            case tipe: BasicType =>
              tipe.entryName +
              formatContainerDebug(value.container)

            // ---------------------------------------------------------------------------
            case nesting: Cls =>
              formatContainerDebug(value.container) + //TODO: ugly
              formatClassDebug(nesting).sectionAllOff }

        // ---------------------------------------------------------------------------
        private def formatContainerDebug(container: Container): String =
          container
            .in.noneIf(_.isOne)
             match {
              case None            => ""
              case Some(container) => s" (${container})" }
}

// ===========================================================================
