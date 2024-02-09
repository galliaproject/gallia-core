package gallia
package reflect

import aptus.{Anything_, String_, Seq_}

// ===========================================================================
case class TypeLeaf(
      name           : FullNameString,  // eg java.lang.String

      dataClass      : Boolean = false, // eg "case class Foo(a: String, b: Int)", but not necessarily all case classes (eg not scala.Some)
      galliaEnumValue: Boolean = false,
      bytes          : Boolean = false, // as ByteBuffer
      inheritsSeq    : Boolean = false,

      enumeratumValueNamesOpt: Option[Seq[String]] = None, // not used currently

      fields: Seq[Field] = Nil) {
    val fullName   : FullyQualifiedName = FullyQualifiedName.from(name) // favor over name now
    def inScopeName: String             = fullName.lastItem

    def keys: Seq[String] = fields.map(_.key)

    // ---------------------------------------------------------------------------
    def formatDebug: String = // TODO: use PPrint
      "[" + name.quote + " - " +
      Seq(
            dataClass      .in.someIf(_ == true).map(_ => "dataClass"),
            galliaEnumValue.in.someIf(_ == true).map(_ => "galliaEnumValue"),
            bytes          .in.someIf(_ == true).map(_ => "bytes"),
            inheritsSeq    .in.someIf(_ == true).map(_ => "inheritsSeq"),

            enumeratumValueNamesOpt.map(_.map(_.quote).join("|")))
          .flatten.join(", ") +
        (if  (fields.isEmpty) "]"
         else                 fields.map(_.formatDebug).section2 + "]")

    // ---------------------------------------------------------------------------
    def isAny: Boolean = this == TypeNodeBuiltIns.ScalaAny.leaf

    def isSeq       : Boolean = inheritsSeq
    def isEnumeratum: Boolean = enumeratumValueNamesOpt.nonEmpty

    def isOption: Boolean = fullName.isOption

    def isNone: Boolean = fullName.isNone
    def isSome: Boolean = fullName.isSome

    def isNotOne: Boolean = isSeq || isOption

    // ===========================================================================
    // TODO: t231017103243 - use lens lib

    // ---------------------------------------------------------------------------
    def dataClass      (value: Boolean): TypeLeaf = copy(dataClass       = value)
    def galliaEnumValue(value: Boolean): TypeLeaf = copy(galliaEnumValue = value)
    def bytes          (value: Boolean): TypeLeaf = copy(bytes           = value)
    def inheritsSeq    (value: Boolean): TypeLeaf = copy(inheritsSeq     = value) }

  // ===========================================================================
  object TypeLeaf {
    val Dummy: TypeLeaf = TypeLeaf.trivial("gallia.Dummy")

    // ---------------------------------------------------------------------------
    lazy val ScalaOption = TypeLeaf.trivial(FullNameBuiltIns._ScalaOption)
    lazy val ScalaSeq    = TypeLeaf.trivial(FullNameBuiltIns._ScalaSeq   ).inheritsSeq(value = true)

    // ---------------------------------------------------------------------------
    def trivial(name: String): TypeLeaf = TypeLeaf(name = name) }

// ===========================================================================
