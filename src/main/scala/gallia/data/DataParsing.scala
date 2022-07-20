package gallia
package data

import aptus._

// ===========================================================================
object DataParsing {
  @inline def parseLocalDate     (value: String) = value.parseLocalDate     /* aptus' */
  @inline def parseLocalTime     (value: String) = value.parseLocalTime     /* aptus' */
  @inline def parseLocalDateTime (value: String) = value.parseLocalDateTime /* aptus' */

  @inline def parseOffsetDateTime(value: String) = value.parseOffsetDateTime /* aptus' */
  @inline def parseZonedDateTime (value: String) = value.parseZonedDateTime  /* aptus' */
  @inline def parseInstant       (value: String) = value.parseInstant        /* aptus' */

  // ---------------------------------------------------------------------------
  def parseBinary(value: String): ByteBuffer =
    value
      .stripPrefixGuaranteed(Base64StringPrefix)
      .unBase64
      .pipe(byteBuffer)
}

// ===========================================================================