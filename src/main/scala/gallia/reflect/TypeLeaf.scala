package gallia.reflect

import aptus.{Anything_, String_, Seq_}

import gallia.Keyz

// ===========================================================================
case class TypeLeaf(
      name       : FullName,
      alias      : Option[FullName] = None,

      dataClass  : Boolean = false, // eg "case class Foo(a: String, b: Int)", but not necessarily all case classes (eg not scala.Some)
      enum       : Boolean = false,
      inheritsSeq: Boolean = false,

      fields     : Seq[Field] = Nil) {

    def keyz: Keyz = fields.map(_.key.symbol).thn(Keyz.apply)

    def isSeq   : Boolean = inheritsSeq

    def isOption: Boolean = name == _Option

    def isNone: Boolean = name == _None
    def isSome: Boolean = name == _Some

    def isNotOne: Boolean = isSeq || isOption

    def unaliased: TypeLeaf = copy(alias = None, fields = fields.map(_.unaliased))

    // ===========================================================================
    override def toString: String = formatDefault
      def formatDefault: String =
        if (fields.isEmpty)     _formatDefault
        else                s"${_formatDefault}${fields.map(_.formatDefault).section2}"

      // ---------------------------------------------------------------------------
      private def _formatDefault: String = s"${name}\t${alias}\t${dataClass}\t${enum}\t${inheritsSeq}"
  }

// ===========================================================================