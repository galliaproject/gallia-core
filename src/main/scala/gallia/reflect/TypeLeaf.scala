package gallia
package reflect

import aptus.{String_, Seq_}

import meta.InfoUtils

// ===========================================================================
case class TypeLeaf(
      name       : FullName,                // eg java.lang.String
      inScopeName: String,                  // eg String
      alias      : Option[FullName] = None, // eg String

      dataClass  : Boolean = false, // eg "case class Foo(a: String, b: Int)", but not necessarily all case classes (eg not scala.Some)
      enm        : Boolean = false, // "enum" is reserved in scala 3
      inheritsSeq: Boolean = false,

      fields     : Seq[Field] = Nil) {

    def keyz: Keyz = fields.map(_.key.symbol).pipe(Keyz.apply)

    def isSeq   : Boolean = inheritsSeq

    def isOption: Boolean = name == _Option

    def isNone: Boolean = name == _None
    def isSome: Boolean = name == _Some

    def isNotOne: Boolean = isSeq || isOption

    def unaliased: TypeLeaf = copy(alias = None, fields = fields.map(_.unaliased))

    // ===========================================================================
    def forceDataClass: Cls =
      dataClassEither match {
        case Left (l) => aptus.illegalArgument(l)
        case Right(r) => r }

    // ---------------------------------------------------------------------------
    def dataClassEither: Either[Any, Cls] = // TODO: use Try until determine precise criteria (cc + not Some + valid types...)
      util.Try(InfoUtils.forceNestedClass(this)) match {
        case util.Failure(error)          => Left(s"TODO:t201015102536:${error.toString}")
        case util.Success(validDataClass) => Right(validDataClass) }
    
    // ===========================================================================
    override def toString: String = formatDefault
      def formatDefault: String =
        if (fields.isEmpty)     _formatDefault
        else                s"${_formatDefault}${fields.map(_.formatDefault).section2}"

      // ---------------------------------------------------------------------------
      private def _formatDefault: String = s"${name}\t${alias}\t${dataClass}\t${enm}\t${inheritsSeq}"
  }

// ===========================================================================