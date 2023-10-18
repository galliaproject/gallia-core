package gallia
package reflect

import aptus.{String_, Seq_}

// ===========================================================================
case class TypeLeaf( // TODO: t231018094951 - capture classOf[T].getName as it doesn't always agree (eg "int") - requires mirror?
      name       : FullNameString,       // eg java.lang.String
      inScopeName: String,               // eg String
      alias      : Option[Alias] = None, // eg String

      dataClass  : Boolean = false, // eg "case class Foo(a: String, b: Int)", but not necessarily all case classes (eg not scala.Some)
      enm        : Boolean = false, // "enum" is reserved in scala 3
      bytes      : Boolean = false, // as ByteBuffer
      inheritsSeq: Boolean = false,

      enumeratumValueNamesOpt: Option[Seq[String]] = None, // not used currently
      fields: Seq[Field] = Nil) {
    val fullName = FullName.from(name)

    def keys: Seq[Key] = fields.map(_.key.symbol)

    // ---------------------------------------------------------------------------
    def isSeq       : Boolean = inheritsSeq
    def isEnumeratum: Boolean = enumeratumValueNamesOpt.nonEmpty

    def isOption: Boolean = fullName.isOption

    def isNone: Boolean = fullName.isNone
    def isSome: Boolean = fullName.isSome

    def isNotOne: Boolean = isSeq || isOption

    // ---------------------------------------------------------------------------    
    def unaliased: TypeLeaf = copy(alias = None, fields = fields.map(_.unaliased))

    // ===========================================================================
    // TODO: t231017103243 - use lens lib
    def alias      (value   :        Alias ): TypeLeaf = copy(alias       = Some(value))
    def alias      (valueOpt: Option[Alias]): TypeLeaf = copy(alias       = valueOpt)
    def inScopeName(value   : InScopeName  ): TypeLeaf = copy(inScopeName = value)

    // ===========================================================================
    override def toString: String = formatDefault
      def formatDefault: String =
        if (fields.isEmpty)     _formatDefault
        else                s"${_formatDefault}${fields.map(_.formatDefault).section2}"

      // ---------------------------------------------------------------------------
      private def _formatDefault: String =
        Seq(
            "inScopeName" ->  inScopeName,
            "alias"       ->  alias,

            "dataClass"   ->  dataClass,
            "enm"         ->  enm,
            "bytes"       ->  bytes,
            "inheritsSeq" ->  inheritsSeq,

            "enumeratumValueNamesOpt" -> enumeratumValueNamesOpt,
            "fields" ->  fields)
          .section(name)
  }

  // ===========================================================================
  object TypeLeaf {
    val Dummy               : TypeLeaf = TypeLeaf.trivial("gallia.Dummy")
    def debug(value: String): TypeLeaf = Dummy.alias(value)

    // ---------------------------------------------------------------------------
    def trivial(name: String, alias: String): TypeLeaf = trivial(name).alias(alias)
    def trivial(name: String)               : TypeLeaf =
      TypeLeaf(
        name        = name,
        inScopeName = name.splitBy(".").last) }

// ===========================================================================
