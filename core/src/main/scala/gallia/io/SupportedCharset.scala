package gallia
package io

import enumeratum.{Enum, EnumEntry}
import java.nio.charset.StandardCharsets
import java.nio.charset.Charset

// ===========================================================================
sealed trait SupportedCharset extends EnumEntry {
    def charset: Charset
    def name = charset.displayName()
  }

  // ===========================================================================
  object SupportedCharset extends Enum[SupportedCharset] { val values = findValues
    type Selector = SupportedCharset.type => SupportedCharset

    // ---------------------------------------------------------------------------
    val Default = UTF_8

    // ---------------------------------------------------------------------------
    /** ported as-is from StandardCharsets per version TODO */
      /** Seven-bit ASCII, a.k.a. ISO646-US, a.k.a. the Basic Latin block of the Unicode character set */ case object US_ASCII   extends SupportedCharset { def charset = StandardCharsets.US_ASCII }
      /** ISO Latin Alphabet No. 1, a.k.a. ISO-LATIN-1                                                 */ case object ISO_8859_1 extends SupportedCharset { def charset = StandardCharsets.ISO_8859_1 }
      /** Eight-bit UCS Transformation Format                                                          */ case object UTF_8      extends SupportedCharset { def charset = StandardCharsets.UTF_8    }
      /** Sixteen-bit UCS Transformation Format, big-endian byte order                                 */ case object UTF_16BE   extends SupportedCharset { def charset = StandardCharsets.UTF_16BE }
      /** Sixteen-bit UCS Transformation Format, little-endian byte order                              */ case object UTF_16LE   extends SupportedCharset { def charset = StandardCharsets.UTF_16LE }
      /** Sixteen-bit UCS Transformation Format, byte order identified by an optional byte-order mark  */ case object UTF_16     extends SupportedCharset { def charset = StandardCharsets.UTF_16   }
  }

// ===========================================================================
