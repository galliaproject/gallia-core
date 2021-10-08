package gallia
package io.out

import enumeratum.{Enum, EnumEntry}
import aptus.{Anything_, String_}

// ===========================================================================
sealed trait OutletType extends EnumEntry { // TODO: rename to *Other*OutletType
    def writeLine:           String  => Unit
    def writeLines: Iterator[String] => Unit }

  // ===========================================================================
  object OutletType extends Enum[OutletType]{
    val values = findValues

    // ===========================================================================
    case class  FormattedString(sw: StringWriter) extends OutletType {
        def writeLine  = string1(sw) _
        def writeLines = string2(sw) _ }

      // ---------------------------------------------------------------------------
      case object StandardOutput extends OutletType {
        def writeLine  = stdout1 _
        def writeLines = stdout2 _ }

      // ---------------------------------------------------------------------------
      case object StandardError  extends OutletType {
        def writeLine  = stderr1 _
        def writeLines = stderr2 _ }

    // ===========================================================================
    def file1(flwc: FileLikeWriteContext)(content: String) { OutputDataUtils.writeFileContent(flwc)(content.newline) }
    def string1(sw: StringWriter)        (content: String) { sw.append(content); () }
    def stdout1                          (content: String) { System.out.println(content) }
    def stderr1                          (content: String) { System.err.println(content) }

    // ---------------------------------------------------------------------------
    def file2(flwc: FileLikeWriteContext)(lines: Iterator[String]) { lines.pipe(OutputDataUtils.writeFileLines(flwc)) }
    def string2(sw: StringWriter)        (lines: Iterator[String]) { lines.map(_.newline).foreach(sw.append) }
    def stdout2                          (lines: Iterator[String]) { lines.foreach(System.out.println) }
    def stderr2                          (lines: Iterator[String]) { lines.foreach(System.err.println) }
  }

// ===========================================================================