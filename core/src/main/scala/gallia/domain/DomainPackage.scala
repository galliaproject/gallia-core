package gallia

import aptus.String_
import aptus.{Separator, Nes, StringValue}

// ===========================================================================
package object domain {
  type PathPair1 = gallia.domain.PathPair
  type KPaths1   = KPath

  // ---------------------------------------------------------------------------
  case class Quintuplet(optional: Boolean, multiple: Boolean, whatever: Boolean, ignoreContainer: Boolean, valueClass: Boolean) // 210122154401; TODO: see t210111095156

  // ===========================================================================
  trait StringSplitter { def apply(value: String): Seq[String] }
      case class StringSplitterFunction (f: StringValue => Nes[StringValue]) extends StringSplitter { def apply(value: String): Seq[String] = f(value) }
      case class StringSplitterSeparator(sep: Separator                    ) extends StringSplitter { def apply(value: String): Seq[String] = value.splitBy(sep) }
      case class StringSplitterRegex    (regex: Regex                      ) extends StringSplitter { def apply(value: String): Seq[String] = value.splitBy(regex) }

    // ---------------------------------------------------------------------------
    object StringSplitter {
      implicit def toStringSplitter(value: StringValue => Nes[StringValue]): StringSplitter = StringSplitterFunction (value)
      implicit def toStringSplitter(value: Separator                      ): StringSplitter = StringSplitterSeparator(value)
      implicit def toStringSplitter(value: Regex                          ): StringSplitter = StringSplitterRegex    (value)
    }

}

// ===========================================================================